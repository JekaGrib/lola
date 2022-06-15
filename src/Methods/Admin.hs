module Methods.Admin where

import Api.Request.QueryStr (CreateUser (..), CreateAdminKey (..), checkQStr, parseQueryStr)
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day)
import Logger
import Methods.Common
import qualified Methods.Common.Exist (Handle, makeH)
import Network.HTTP.Types (QueryText)
import Error (ReqError (..), hideErr)
import Psql.Methods.Admin
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectKeys :: m [Key],
    insertReturnUser :: InsertUser -> m UserId,
    getDay :: m Day,
    generateTokenKey :: m TokenKey,
    hExist :: Methods.Common.Exist.Handle m
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
        generateTokenKey'
        (Methods.Common.Exist.makeH conf)

workWithAdmin :: (MonadCatch m) => Handle m -> QueryText -> ExceptT ReqError m ResponseInfo
workWithAdmin h@Handle {..} qStr = do
  hideErr $ lift $ logInfo hLog "Create admin command"
  checkAdminKey h qStr
  checkQStr hExist qStr >>= createAdmin h

createAdmin :: (MonadCatch m) => Handle m -> CreateUser -> ExceptT ReqError m ResponseInfo
createAdmin Handle {..} (CreateUser pwdParam fNameParam lNameParam picIdParam) = do
  day <- lift getDay
  let hashPwdParam = pack . strSha1 . unpack $ pwdParam
  tokenKey <- lift generateTokenKey
  let insUser = InsertUser hashPwdParam fNameParam lNameParam picIdParam day True tokenKey
  admId <- catchInsRetE hLog $ insertReturnUser insUser
  let usToken = pack $ show admId ++ ".hij." ++ strSha1 ("hij" ++ tokenKey)
  lift $ logInfo hLog $ "User_id: " ++ show admId ++ " created as admin"
  ok201UserHelper hConf usToken admId 

checkAdminKey :: (MonadCatch m) => Handle m -> QueryText -> ExceptT ReqError m ()
checkAdminKey Handle {..} qStr = hideErr $ do 
  CreateAdminKey keyParam <- parseQueryStr qStr
  keys <- catchSelE hLog selectKeys
  key <- getLastKey keys
  compareKeysE keyParam key

compareKeysE :: (MonadCatch m) => QueryTxtParam -> Text -> ExceptT ReqError m ()
compareKeysE keyParam key
  | keyParam == key = return ()
  | otherwise = throwE $ BadReqError "Invalid create_admin_key"

getLastKey :: (MonadCatch m) => [Text] -> ExceptT ReqError m Text
getLastKey [] = throwE $ DatabaseError "Empty output. Table key is empty."
getLastKey xs = return $ last xs
