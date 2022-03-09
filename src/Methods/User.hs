{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Methods.User where

import Api.Response (OkResponse (..), UserResponse (..), UserTokenResponse (..),TokenResponse(..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT,throwE)
import Data.Text (pack)
import Database.PostgreSQL.Simple (withTransaction)
import Logger
import Methods.Common
import Methods.Common.DeleteMany (deleteAllAboutDrafts)
import qualified Methods.Common.DeleteMany (Handle, makeH)
import Methods.Common.Selecty (User (..),Auth(..))
import Oops (ReqError(..),hideLogInErr)
import Api.Request.QueryStr (CreateUser (..),LogIn(..),checkQStr)
import Types
import Data.Time.Calendar ( Day)
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE,UncheckedExId(..))
import Methods.Common.ToQuery
import Network.HTTP.Types (StdMethod(..))
import TryRead (tryReadResourseId)

data Handle m = Handle
  { hConf :: Config
  , hLog :: LogHandle m
  , selectUsers :: UserId -> m [User]
  , selectAuthsForUser :: UserId -> m [Auth]
  , selectAuthorsForUser :: UserId -> m [AuthorId]
  , selectDraftsForAuthor :: AuthorId -> m [DraftId]
  , updateDbUserForComms :: UserId -> UserId -> m ()
  , updateDbAuthorForPosts :: AuthorId -> AuthorId -> m ()
  , updateDbTokenKeyForUser :: TokenKey -> UserId -> m ()
  , deleteDbUser :: UserId -> m ()
  , deleteDbAuthor :: AuthorId -> m ()
  , insertReturnUser :: InsertUser -> m UserId
  , getDay :: m Day
  , getTokenKey :: m TokenKey
  , withTransactionDB :: forall a. m a -> m a
  , hDelMany :: Methods.Common.DeleteMany.Handle m
  , hAuth :: Methods.Common.Auth.Handle m
  , hExist :: Methods.Common.Exist.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectUsers' conn)
        (selectAuthsForUser' conn)
        (selectAuthorsForUser' conn)
        (selectDraftsForAuthor' conn)
        (updateDbUserForComms' conn)
        (updateDbAuthorForPosts' conn)
        (updateDbTokenKeyForUser' conn)
        (deleteDbUser' conn)
        (deleteDbAuthor' conn)
        (insertReturnUser' conn)
        getDay'
        getTokenKey'
        (withTransaction conn)
        (Methods.Common.DeleteMany.makeH conf)
        (Methods.Common.Auth.makeH conf logH)
        (Methods.Common.Exist.makeH conf)


selectUsers' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  select' conn $
    Select 
      ["first_name", "last_name", "user_pic_id", "user_create_date"]
      "users" 
      wh
selectAuthsForUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  select' conn $ Select ["password", "admin"] "users" wh
selectAuthorsForUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  selectOnly' conn $ Select ["author_id"] "authors" wh
selectDraftsForAuthor' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  selectOnly' conn $ Select ["draft_id"] "drafts" wh
updateDbUserForComms' conn newUsId usId = do
  let set = SetPair "user_id=?" (Id newUsId)
  let wh = WherePair "user_id=?" (Id usId)
  updateInDb' conn (Update "comments" [set] wh)
updateDbAuthorForPosts' conn newAuId auId = do
  let set = SetPair "author_id=?" (Id newAuId)
  let wh = WherePair "author_id=?" (Id auId)
  updateInDb' conn (Update "posts" [set] wh)
updateDbTokenKeyForUser' conn tokenKey usId = do
  let set = SetPair "token_key=?" (Str tokenKey)
  let wh = WherePair "user_id=?" (Id usId)
  updateInDb' conn (Update "users" [set] wh)
deleteDbUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  deleteFromDb' conn (Delete "users" wh)
deleteDbAuthor' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  deleteFromDb' conn (Delete "authors" wh)
insertReturnUser' conn (InsertUser pwd fName lName picId day bool tokenKey) = do
  let insPair1 = InsertPair "password"         (Txt  pwd)
  let insPair2 = InsertPair "first_name"       (Txt  fName)
  let insPair3 = InsertPair "last_name"        (Txt  lName)
  let insPair4 = InsertPair "user_pic_id"      (Id   picId)
  let insPair5 = InsertPair "user_create_date" (Day  day)
  let insPair6 = InsertPair "admin"            (Bool bool)
  let insPair7 = InsertPair "token_key"        (Str  tokenKey)
  let insPairs = [insPair1,insPair2,insPair3,insPair4,insPair5,insPair6,insPair7]
  insertReturn' conn (InsertRet "users" insPairs "user_id")

workWithUsers :: (MonadCatch m) => Handle m -> ReqInfo -> ExceptT ReqError m ResponseInfo
workWithUsers h@Handle{..} (ReqInfo meth path qStr _) = 
  case (meth,path) of
    (POST,["users","logIn"]) -> hideLogInErr $ do
      lift $ logInfo hLog "LogIn command"
      checkQStr hExist qStr >>= logIn h
    (POST,["users"]) -> do
      lift $ logInfo hLog "Create user command"
      checkQStr hExist qStr >>= createUser h
    (GET,["users",usIdTxt]) -> do
      lift $ logInfo hLog "Get user command"
      usId <- checkUserResourse h usIdTxt
      getUser h usId
    (DELETE,["users",usIdTxt]) -> do
      lift $ logInfo hLog "Delete user command"
      tokenAdminAuth hAuth qStr
      usId <- checkUserResourse h usIdTxt
      deleteUser h usId
    (x,y) -> throwE $ ResourseNotExistError $ "Unknown method-path combination: " ++ show (x,y)

logIn :: (MonadCatch m) => Handle m -> LogIn -> ExceptT ReqError m ResponseInfo
logIn Handle{..} (LogIn usIdParam pwdParam) = do
  Auth pwd admBool <- catchOneSelE hLog $ selectAuthsForUser usIdParam
  checkPwd pwdParam pwd
  tokenKey <- lift $ getTokenKey 
  catchUpdE hLog $ updateDbTokenKeyForUser tokenKey usIdParam
  if admBool
    then do
      let usToken = pack $ show usIdParam ++ ".hij." ++ strSha1 ("hij" ++ tokenKey)
      lift $ logInfo hLog $ "User_id: " ++ show usIdParam ++ " successfully logIn as admin."
      okHelper $ TokenResponse {tokenTR = usToken}
    else do
      let usToken = pack $ show usIdParam ++ ".stu." ++ strSha1 ("stu" ++ tokenKey)
      lift $ logInfo hLog $ "User_id: " ++ show usIdParam ++ " successfully logIn as user."
      okHelper $ TokenResponse {tokenTR = usToken}

createUser :: (MonadCatch m) => Handle m -> CreateUser -> ExceptT ReqError m ResponseInfo
createUser Handle{..} (CreateUser pwdParam fNameParam lNameParam picIdParam) = do
  day <- lift $ getDay 
  tokenKey <- lift $ getTokenKey 
  let hashPwdParam = txtSha1 pwdParam
  let insUser = InsertUser hashPwdParam fNameParam lNameParam picIdParam day False tokenKey
  usId <- catchInsRetE hLog $ insertReturnUser insUser
  lift $ logDebug hLog $ "DB return user_id:" ++ show usId ++ "and token key"
  lift $ logInfo hLog $ "User_id: " ++ show usId ++ " created"
  let usToken = pack $ show usId ++ "." ++ strSha1 tokenKey ++ ".stu." ++ strSha1 ("stu" ++ tokenKey)
  okHelper $ UserTokenResponse {tokenUTR = usToken, user_idUTR = usId, first_nameUTR = fNameParam, last_nameUTR = lNameParam, user_pic_idUTR = picIdParam, user_pic_urlUTR = makeMyPicUrl hConf picIdParam, user_create_dateUTR = day}

getUser :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m ResponseInfo
getUser Handle{..} usId = do
  User fName lName picId usCreateDate <- catchOneSelE hLog $ selectUsers usId
  okHelper $ UserResponse {user_id = usId, first_name = fName, last_name = lName, user_pic_id = picId, user_pic_url = makeMyPicUrl hConf picId, user_create_date = usCreateDate}

deleteUser :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m ResponseInfo
deleteUser h@Handle{..} usId = do
  let updateCom = updateDbUserForComms (cDefUsId hConf) usId
  let deleteUs = deleteDbUser usId
  maybeAuId <- catchMaybeOneSelE hLog $ selectAuthorsForUser usId
  case maybeAuId of
    Just authorId -> do
      let updatePost = updateDbAuthorForPosts (cDefAuthId hConf ) authorId 
      draftsIds <- catchSelE hLog $ selectDraftsForAuthor authorId
      let deleteDr = deleteAllAboutDrafts hDelMany draftsIds
      let deleteAu = deleteDbAuthor authorId
      withTransactionDBE h (updateCom >> updatePost >> deleteDr >> deleteAu >> deleteUs)
    Nothing ->
      withTransactionDBE h (updateCom >> deleteUs)
  lift $ logInfo hLog $ "User_id: " ++ show usId ++ " deleted"
  okHelper $ OkResponse {ok = True}

checkPwd :: (MonadCatch m) => Pwd -> Pwd -> ExceptT ReqError m ()
checkPwd pwdParam pwd
  | pwd == hashPwdParam = return ()
  | otherwise = throwE . SecretLogInError $ "INVALID password"
  where
    hashPwdParam = txtSha1 pwdParam

checkUserResourse :: (MonadCatch m) =>  Handle m -> ResourseId -> ExceptT ReqError m UserId
checkUserResourse Handle{..} usIdTxt = do
  iD <- tryReadResourseId "user_id" usIdTxt
  isExistResourseE hExist (UserId iD)
  return iD

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchTransactE (hLog h) . withTransactionDB h


