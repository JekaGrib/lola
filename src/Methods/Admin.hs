--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}




module Methods.Admin where
          
import           Api.Response
import           Logger
import           Types
import           Oops
import           Methods.Handle
import ParseQueryStr (CreateAdmin(..))
import Conf (Config(..))
import           Network.Wai (Request,ResponseReceived,Response,responseBuilder,strictRequestBody,pathInfo)
import           Data.Text                      ( pack, unpack, Text )
import           Database.PostgreSQL.Simple (query, withTransaction, execute, executeMany,Connection,Only(..),Binary(Binary))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( catch, throwM, MonadCatch)


createAdmin :: (MonadCatch m) => MethodsHandle m -> CreateAdmin -> ExceptT ReqError m ResponseInfo
createAdmin h (CreateAdmin keyParam pwdParam fNameParam lNameParam picIdNum) = do
  let picIdParam = numToTxt picIdNum
  onlyKeys <- selectListFromDbE h "key" ["create_admin_key"] "true" ([]::[Text])  
  let keys = fmap fromOnly onlyKeys
  checkEmptyList keys
  checkKeyE keyParam (last keys)
  day   <- lift $ getDay h
  let hashPwdParam = pack . strSha1 . unpack $ pwdParam
  tokenKey <- lift $ getTokenKey h
  let insNames  = ["password","first_name","last_name","user_pic_id"    ,"user_create_date","admin","token_key"]
  let insValues = [hashPwdParam  ,fNameParam  ,lNameParam ,picIdParam,pack day          ,"TRUE",pack tokenKey ]
  admId <-  insertReturnE h "users" "user_id" insNames insValues 
  lift $ logDebug (hLog h) $ "DB return user_id" ++ show admId
  lift $ logInfo (hLog h) $ "User_id: " ++ show admId ++ " created as admin"
  let usToken = pack $ show admId ++ "." ++ strSha1 tokenKey ++ ".hij." ++ strSha1 ("hij" ++ tokenKey)
  okHelper $ UserTokenResponse {tokenUTR = usToken, user_idUTR = admId, first_nameUTR = fNameParam, last_nameUTR = lNameParam, user_pic_idUTR = picIdNum, user_pic_urlUTR = makeMyPicUrl picIdNum, user_create_dateUTR = pack day }

checkKeyE :: (MonadCatch m) => Text -> Text -> ExceptT ReqError m ()
checkKeyE keyParam key 
  | keyParam == key = return ()
  | otherwise       = throwE $ SimpleError "Invalid create_admin_key"

checkEmptyList :: (MonadCatch m) => [Text] -> ExceptT ReqError m ()
checkEmptyList [] = throwE $ SimpleError "DatabaseError.Empty output"
checkEmptyList _  = return ()