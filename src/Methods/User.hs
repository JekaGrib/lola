{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.User where

import Api.Response (OkResponse (..), UserResponse (..), UserTokenResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (pack)
import Database.PostgreSQL.Simple (withTransaction)
import Logger
import Methods.Common
import Methods.Common.DeleteMany (deleteAllAboutDrafts)
import qualified Methods.Common.DeleteMany (Handle, makeH)
import Methods.Common.Selecty (User (..))
import Oops (ReqError)
import ParseQueryStr (CreateUser (..), DeleteUser (..))
import Types
import Data.Time.Calendar ( Day)


data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectNums :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Id],
    selectUsers :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [User],
    updateInDb :: Table -> ToUpdate -> Where -> [DbValue] -> m (),
    deleteFromDb :: Table -> Where -> [DbValue] -> m (),
    isExistInDb :: Table -> Where -> DbValue -> m Bool,
    insertReturn :: Table -> DbReturnParamKey -> [DbInsertParamKey] -> [DbValue] -> m Id,
    getDay :: m Day,
    getTokenKey :: m String,
    withTransactionDB :: forall a. m a -> m a,
    hDelMany :: Methods.Common.DeleteMany.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectOnly' conn)
        (select' conn)
        (updateInDb' conn)
        (deleteFromDb' conn)
        (isExistInDb' conn)
        (insertReturn' conn)
        getDay'
        getTokenKey'
        (withTransaction conn)
        (Methods.Common.DeleteMany.makeH conf)

createUser :: (MonadCatch m) => Handle m -> CreateUser -> ExceptT ReqError m ResponseInfo
createUser h (CreateUser pwdParam fNameParam lNameParam picIdParam) = do
  day <- lift $ getDay h
  tokenKey <- lift $ getTokenKey h
  let hashPwdParam = txtSha1 pwdParam
  isExistInDbE h "pics" "pic_id=?" (Id picIdParam)
  let insNames = ["password", "first_name", "last_name", "user_pic_id", "user_create_date", "admin", "token_key"]
  let insValues = [Txt hashPwdParam, Txt fNameParam, Txt lNameParam, Id picIdParam, Day day, Bool False, Txt (pack tokenKey)]
  usId <- insertReturnE h "users" "user_id" insNames insValues
  lift $ logDebug (hLog h) $ "DB return user_id:" ++ show usId ++ "and token key"
  lift $ logInfo (hLog h) $ "User_id: " ++ show usId ++ " created"
  let usToken = pack $ show usId ++ "." ++ strSha1 tokenKey ++ ".stu." ++ strSha1 ("stu" ++ tokenKey)
  okHelper $ UserTokenResponse {tokenUTR = usToken, user_idUTR = usId, first_nameUTR = fNameParam, last_nameUTR = lNameParam, user_pic_idUTR = picIdParam, user_pic_urlUTR = makeMyPicUrl (hConf h) picIdParam, user_create_dateUTR = day}

getUser :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m ResponseInfo
getUser h usIdNum = do
  let selectParams = ["first_name", "last_name", "user_pic_id", "user_create_date"]
  User fName lName picId usCreateDate <- checkOneIfExistE (hLog h) (selectUsers h) "users" selectParams "user_id=?" (Id usIdNum)
  okHelper $ UserResponse {user_id = usIdNum, first_name = fName, last_name = lName, user_pic_id = picId, user_pic_url = makeMyPicUrl (hConf h) picId, user_create_date = usCreateDate}

deleteUser :: (MonadCatch m) => Handle m -> DeleteUser -> ExceptT ReqError m ResponseInfo
deleteUser h (DeleteUser usIdParam) = do
  isExistInDbE h "users" "user_id=?" (Id usIdParam)
  let updateCom = updateInDb h "comments" "user_id=?" "user_id=?" [Id $ cDefUsId (hConf h), Id usIdParam]
  let deleteUs = deleteFromDb h "users" "user_id=?" [Id usIdParam]
  maybeAuId <- checkMaybeOneE (hLog h) $ selectNums h "authors" ["author_id"] "user_id=?" [Id usIdParam]
  case maybeAuId of
    Just authorId -> do
      let updatePost = updateInDb h "posts" "author_id=?" "author_id=?" [Id $ cDefAuthId (hConf h), Id authorId ]
      draftsIds <- checkListE (hLog h) $ selectNums h "drafts" ["draft_id"] "author_id=?" [Id authorId]
      let deleteDr = deleteAllAboutDrafts (hDelMany h) draftsIds
      let deleteAu = deleteFromDb h "authors" "author_id=?" [Id authorId]
      withTransactionDBE h (updateCom >> updatePost >> deleteDr >> deleteAu >> deleteUs)
    Nothing ->
      withTransactionDBE h (updateCom >> deleteUs)
  lift $ logInfo (hLog h) $ "User_id: " ++ show usIdParam ++ " deleted"
  okHelper $ OkResponse {ok = True}

isExistInDbE :: (MonadCatch m) => Handle m -> Table -> Where -> DbValue -> ExceptT ReqError m ()
isExistInDbE h = checkIsExistE (hLog h) (isExistInDb h)

insertReturnE :: (MonadCatch m) => Handle m -> Table -> String -> [String] -> [DbValue] -> ExceptT ReqError m Id
insertReturnE h = checkInsRetE (hLog h) (insertReturn h)

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = checkTransactE (hLog h) . withTransactionDB h
