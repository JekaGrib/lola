{-# LANGUAGE RankNTypes #-}

module Methods.User where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateUser (..), LogIn (..), checkQStr)
import Api.Response (TokenResponse (..), TokenResponse (..), UserResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Text (pack)
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple (withTransaction)
import Logger
import Methods.Common
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth)
import Methods.Common.DeleteMany (deleteAllAboutDrafts)
import qualified Methods.Common.DeleteMany (Handle, makeH)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Network.HTTP.Types (QueryText)
import Error (ReqError (..), hideLogInErr)
import Psql.Methods.User
import Psql.Selecty (Auth (..), User (..))
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectUsers :: UserId -> m [User],
    selectAuthsForUser :: UserId -> m [Auth],
    selectAuthorsForUser :: UserId -> m [AuthorId],
    selectDraftsForAuthor :: AuthorId -> m [DraftId],
    updateDbUserForComms :: UserId -> UserId -> m (),
    updateDbAuthorForPosts :: AuthorId -> AuthorId -> m (),
    updateDbTokenKeyForUser :: TokenKey -> UserId -> m (),
    deleteDbUser :: UserId -> m (),
    deleteDbAuthor :: AuthorId -> m (),
    insertReturnUser :: InsertUser -> m UserId,
    getDay :: m Day,
    generateTokenKey :: m TokenKey,
    withTransactionDB :: forall a. m a -> m a,
    hDelMany :: Methods.Common.DeleteMany.Handle m,
    hAuth :: Methods.Common.Auth.Handle m,
    hExist :: Methods.Common.Exist.Handle m
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
        generateTokenKey'
        (withTransaction conn)
        (Methods.Common.DeleteMany.makeH conf)
        (Methods.Common.Auth.makeH conf logH)
        (Methods.Common.Exist.makeH conf)

workWithLogIn :: (MonadCatch m) => Handle m -> QueryText -> ExceptT ReqError m ResponseInfo
workWithLogIn h@Handle {..} qStr = hideLogInErr $ do
  lift $ logInfo hLog "LogIn command"
  checkQStr hExist qStr >>= logIn h

workWithUsers :: (MonadCatch m) => Handle m -> QueryText -> AppMethod -> ExceptT ReqError m ResponseInfo
workWithUsers h@Handle {..} qStr meth =
  case meth of
    ToPost -> do
      lift $ logInfo hLog "Create user command"
      checkQStr hExist qStr >>= createUser h
    ToGet usId -> do
      lift $ logInfo hLog "Get user command"
      isExistResourseE hExist (UserId usId)
      getUser h usId
    ToDelete usId -> do
      lift $ logInfo hLog "Delete user command"
      tokenAdminAuth hAuth qStr
      isExistResourseE hExist (UserId usId)
      deleteUser h usId
    _ -> throwE $ ResourseNotExistError $ "Wrong method for users resourse: " ++ show meth

logIn :: (MonadCatch m) => Handle m -> LogIn -> ExceptT ReqError m ResponseInfo
logIn Handle {..} (LogIn usIdParam pwdParam) = do
  Auth pwd admBool <- catchOneSelE hLog $ selectAuthsForUser usIdParam
  checkPwd pwdParam pwd
  tokenKey <- lift generateTokenKey
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
createUser Handle {..} (CreateUser pwdParam fNameParam lNameParam picIdParam) = do
  day <- lift getDay
  tokenKey <- lift generateTokenKey
  let hashPwdParam = txtSha1 pwdParam
  let insUser = InsertUser hashPwdParam fNameParam lNameParam picIdParam day False tokenKey
  usId <- catchInsRetE hLog $ insertReturnUser insUser
  lift $ logDebug hLog $ "DB return user_id:" ++ show usId ++ "and token key"
  lift $ logInfo hLog $ "User_id: " ++ show usId ++ " created"
  let usToken = pack $ show usId ++ ".stu." ++ strSha1 ("stu" ++ tokenKey)
  ok201JsonHelper hConf ("users/" ++ show usId) $ TokenResponse {tokenTR = usToken}

getUser :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m ResponseInfo
getUser Handle {..} usId = do
  User fName lName picId usCreateDate <- catchOneSelE hLog $ selectUsers usId
  okHelper $ UserResponse {user_id = usId, first_name = fName, last_name = lName, user_pic_id = picId, user_pic_url = makeMyPicUrl hConf picId, user_create_date = usCreateDate}

deleteUser :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m ResponseInfo
deleteUser h@Handle {..} usId = do
  let updateCom = updateDbUserForComms (cDefUsId hConf) usId
  let deleteUs = deleteDbUser usId
  maybeAuId <- catchMaybeOneSelE hLog $ selectAuthorsForUser usId
  case maybeAuId of
    Just authorId -> do
      let updatePost = updateDbAuthorForPosts (cDefAuthId hConf) authorId
      draftsIds <- catchSelE hLog $ selectDraftsForAuthor authorId
      let deleteDr = deleteAllAboutDrafts hDelMany draftsIds
      let deleteAu = deleteDbAuthor authorId
      withTransactionDBE h (updateCom >> updatePost >> deleteDr >> deleteAu >> deleteUs)
    Nothing ->
      withTransactionDBE h (updateCom >> deleteUs)
  lift $ logInfo hLog $ "User_id: " ++ show usId ++ " deleted"
  ok204Helper

checkPwd :: (MonadCatch m) => Pwd -> Pwd -> ExceptT ReqError m ()
checkPwd pwdParam pwd
  | pwd == hashPwdParam = return ()
  | otherwise = throwE . SecretLogInError $ "INVALID password"
  where
    hashPwdParam = txtSha1 pwdParam

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchTransactE (hLog h) . withTransactionDB h
