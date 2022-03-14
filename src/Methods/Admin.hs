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
import Methods.Common.Exist (isExistResourseE)
import Methods.Common.Exist.UncheckedExId (UncheckedExId(..))
import Psql.ToQuery
import Network.HTTP.Types (StdMethod(..),QueryText)
import Api.Request.EndPoint
import Psql.Methods.Admin


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


workWithAdmin :: (MonadCatch m) => Handle m -> QueryText -> ExceptT ReqError m ResponseInfo
workWithAdmin h@Handle{..} qStr = hideErr $ do
  lift $ logInfo hLog "Create admin command"
  checkQStr hExist qStr >>= createAdmin h

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

