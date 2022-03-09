{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Methods.Admin where

import Api.Response (UserTokenResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Text (Text, pack, unpack)
import Logger
import Methods.Common
import Oops
import Api.Request.QueryStr (CreateAdmin (..),checkQStr)
import Types
import Data.Time.Calendar ( Day)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE,UncheckedExId(..))
import Methods.Common.ToQuery
import Network.HTTP.Types (StdMethod(..))


data Handle m = Handle
  { hConf :: Config
  , hLog :: LogHandle m
  , selectKeys :: m [Key]
  , insertReturnUser :: InsertUser -> m UserId
  , getDay :: m Day
  , getTokenKey :: m TokenKey
  , hExist :: Methods.Common.Exist.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectKeys' conn)
        (insertReturnUser' conn)
        getDay'
        getTokenKey'
        (Methods.Common.Exist.makeH conf)

selectKeys' conn = 
  selectOnly' conn $ Select ["create_admin_key"] "key" (Where "true")
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

workWithAdmin :: (MonadCatch m) => Handle m -> ReqInfo -> ExceptT ReqError m ResponseInfo
workWithAdmin h@Handle{..} (ReqInfo meth path qStr _) = 
  case (meth,path) of
    (POST,["users","admin"]) -> hideErr $ do
      lift $ logInfo hLog "Create admin command"
      checkQStr hExist qStr >>= createAdmin h
    (x,y) -> throwE $ ResourseNotExistError $ "Unknown method-path combination: " ++ show (x,y)

createAdmin :: (MonadCatch m) => Handle m -> CreateAdmin -> ExceptT ReqError m ResponseInfo
createAdmin Handle{..} (CreateAdmin keyParam pwdParam fNameParam lNameParam picIdParam) = do
  keys <- catchSelE hLog selectKeys 
  key <- getLastKey keys
  checkKeyE keyParam key
  day <- lift $ getDay 
  let hashPwdParam = pack . strSha1 . unpack $ pwdParam
  tokenKey <- lift $ getTokenKey 
  let insUser = InsertUser hashPwdParam fNameParam lNameParam picIdParam day True tokenKey
  admId <- catchInsRetE hLog $ insertReturnUser insUser
  let usToken = pack $ show admId ++ "." ++ strSha1 tokenKey ++ ".hij." ++ strSha1 ("hij" ++ tokenKey)
  lift $ logInfo hLog $ "User_id: " ++ show admId ++ " created as admin"
  okHelper $ UserTokenResponse {tokenUTR = usToken, user_idUTR = admId, first_nameUTR = fNameParam, last_nameUTR = lNameParam, user_pic_idUTR = picIdParam, user_pic_urlUTR = makeMyPicUrl hConf picIdParam, user_create_dateUTR = day}

checkKeyE :: (MonadCatch m) => QueryTxtParam -> Text -> ExceptT ReqError m ()
checkKeyE keyParam key
  | keyParam == key = return ()
  | otherwise = throwE $ BadReqError "Invalid create_admin_key"

getLastKey :: (MonadCatch m) => [Text] -> ExceptT ReqError m Text
getLastKey [] = throwE $ DatabaseError "Empty output. Table key is empty."
getLastKey xs = return $ last xs

