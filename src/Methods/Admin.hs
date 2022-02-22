{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

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
import ParseQueryStr (CreateAdmin (..))
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectTxts :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Text],
    insertReturn :: Table -> DbReturnParamKey -> [DbInsertParamKey] -> [DbValue] -> m Integer,
    getDay :: m String,
    getTokenKey :: m String
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectOnly' conn)
        (insertReturn' conn)
        getDay'
        getTokenKey'

createAdmin :: (MonadCatch m) => Handle m -> CreateAdmin -> ExceptT ReqError m ResponseInfo
createAdmin h (CreateAdmin keyParam pwdParam fNameParam lNameParam picIdParam) = do
  keys <- checkListE (hLog h) $ selectTxts h "key" ["create_admin_key"] "true" [] 
  checkEmptyList keys
  checkKeyE keyParam (last keys)
  day <- lift $ getDay h
  let hashPwdParam = pack . strSha1 . unpack $ pwdParam
  tokenKey <- lift $ getTokenKey h
  let insNames = ["password", "first_name", "last_name", "user_pic_id", "user_create_date", "admin", "token_key"]
  let insValues = [Txt hashPwdParam, Txt fNameParam, Txt lNameParam,Num picIdParam, Txt (pack day), Txt "TRUE", Txt (pack tokenKey)]
  admId <- insertReturnE h "users" "user_id" insNames insValues
  let usToken = pack $ show admId ++ "." ++ strSha1 tokenKey ++ ".hij." ++ strSha1 ("hij" ++ tokenKey)
  lift $ logInfo (hLog h) $ "User_id: " ++ show admId ++ " created as admin"
  okHelper $ UserTokenResponse {tokenUTR = usToken, user_idUTR = admId, first_nameUTR = fNameParam, last_nameUTR = lNameParam, user_pic_idUTR = picIdParam, user_pic_urlUTR = makeMyPicUrl (hConf h) picIdParam, user_create_dateUTR = pack day}

checkKeyE :: (MonadCatch m) => QueryTxtParam -> Text -> ExceptT ReqError m ()
checkKeyE keyParam key
  | keyParam == key = return ()
  | otherwise = throwE $ SimpleError "Invalid create_admin_key"

checkEmptyList :: (MonadCatch m) => [Text] -> ExceptT ReqError m ()
checkEmptyList [] = throwE $ SimpleError "DatabaseError.Empty output"
checkEmptyList _ = return ()

insertReturnE :: (MonadCatch m) => Handle m -> Table -> String -> [String] -> [DbValue] -> ExceptT ReqError m Integer
insertReturnE h = checkInsRetE (hLog h) (insertReturn h)
