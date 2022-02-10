{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module Methods.Admin where
          
import           Api.Response (UserTokenResponse(..))
import           Logger
import           Types
import           Oops
import           Methods.Common
import ParseQueryStr (CreateAdmin(..))
import           Data.Text                      ( pack, unpack, Text )
import           Control.Monad.Trans.Except (ExceptT,throwE)
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( MonadCatch)
import  Conf (Config(..),extractConn)



data Handle m = Handle 
  { hConf              :: Config,
    hLog               :: LogHandle m ,
    selectTxt          :: Table -> [String] -> String -> [Text] -> m [Text],
    insertReturn       :: Table -> String -> [String] -> [Text] -> m Integer,
    getDay             :: m String,
    getTokenKey        :: m String
    }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH = let conn = extractConn conf in
  Handle 
    conf 
    logH
    (selectOnly' conn) 
    (insertReturn' conn)
    getDay'   
    getTokenKey'

createAdmin :: (MonadCatch m) => Handle m -> CreateAdmin -> ExceptT ReqError m ResponseInfo
createAdmin h (CreateAdmin keyParam pwdParam fNameParam lNameParam picIdNum) = do
  let picIdParam = numToTxt picIdNum
  keys <- checkListE (hLog h) $ selectTxt h "key" ["create_admin_key"] "true" ([]::[Text])  
  checkEmptyList keys
  checkKeyE keyParam (last keys)
  day   <- lift $ getDay h
  let hashPwdParam = pack . strSha1 . unpack $ pwdParam
  tokenKey <- lift $ getTokenKey h
  let insNames  = ["password","first_name","last_name","user_pic_id"    ,"user_create_date","admin","token_key"]
  let insValues = [hashPwdParam  ,fNameParam  ,lNameParam ,picIdParam,pack day          ,"TRUE",pack tokenKey ]
  admId <- insertReturnE h "users" "user_id" insNames insValues 
  let usToken = pack $ show admId ++ "." ++ strSha1 tokenKey ++ ".hij." ++ strSha1 ("hij" ++ tokenKey)
  lift $ logInfo (hLog h) $ "User_id: " ++ show admId ++ " created as admin"
  okHelper $ UserTokenResponse {tokenUTR = usToken, user_idUTR = admId, first_nameUTR = fNameParam, last_nameUTR = lNameParam, user_pic_idUTR = picIdNum, user_pic_urlUTR = makeMyPicUrl (hConf h) picIdNum, user_create_dateUTR = pack day }

checkKeyE :: (MonadCatch m) => QueryTxtParam -> Text -> ExceptT ReqError m ()
checkKeyE keyParam key 
  | keyParam == key = return ()
  | otherwise       = throwE $ SimpleError "Invalid create_admin_key"

checkEmptyList :: (MonadCatch m) => [Text] -> ExceptT ReqError m ()
checkEmptyList [] = throwE $ SimpleError "DatabaseError.Empty output"
checkEmptyList _  = return ()

insertReturnE :: (MonadCatch m) => Handle m -> Table -> String -> [String] -> [Text] -> ExceptT ReqError m Integer
insertReturnE h = checkInsRetE (hLog h) (insertReturn h) 