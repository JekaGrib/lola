{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module Methods.Auth where
          
import           Api.Response (TokenResponse(..))
import           Logger
import           Types
import           Oops
import           Methods.Common
import Methods.Common.Select (Auth(..))
import TryRead (tryReadNum)
import ParseQueryStr (Token(..),LogIn(..),parseQueryStr)
import           Network.Wai (Request)
import           Data.Text                      ( pack, unpack, Text )
import           Control.Monad.Trans.Except (ExceptT,throwE)
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( MonadCatch)
import  Conf (Config(..),extractConn)


data Handle m = Handle 
  { hConf              :: Config,
    hLog               :: LogHandle m ,
    selectTxts          :: Table -> [Param] -> Where -> [Text] -> m [Text],
    selectAuths         :: Table -> [Param] -> Where -> [Text] -> m [Auth],
    updateInDb         :: Table -> String -> String -> [Text] -> m (),
    getTokenKey        :: m String
    }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH = let conn = extractConn conf in
  Handle 
    conf 
    logH 
    (selectOnly' conn)  
    (select' conn)
    (updateInDb' conn) 
    getTokenKey' 

logIn :: (MonadCatch m) => Handle m -> LogIn -> ExceptT ReqError m ResponseInfo
logIn h (LogIn usIdNum pwdParam) = do
  let usIdParam = numToTxt usIdNum
  Auth pwd admBool <- checkOneIfExistE (hLog h) (selectAuths h) "users" ["password","admin"] "user_id=?" usIdParam 
  checkPwd pwdParam pwd
  tokenKey <- lift $ getTokenKey h
  updateInDbE h "users" "token_key=?" "user_id=?" [pack tokenKey,usIdParam]
  if admBool
    then do 
      let usToken = pack $ show usIdNum ++ "." ++ strSha1 tokenKey ++ ".hij." ++ strSha1 ("hij" ++ tokenKey)
      lift $ logInfo (hLog h) $ "User_id: " ++ show usIdNum ++ " successfully logIn as admin."
      okHelper $ TokenResponse {tokenTR = usToken}  
    else do
      let usToken = pack $ show usIdNum ++ "." ++ strSha1 tokenKey ++ ".stu." ++ strSha1 ("stu" ++ tokenKey)
      lift $ logInfo (hLog h) $ "User_id: " ++ show usIdNum ++ " successfully logIn as user."
      okHelper $ TokenResponse {tokenTR = usToken}


type UserAccessMode = (UserId,AccessMode)
data AccessMode = UserMode | AdminMode

tokenAdminAuth :: (MonadCatch m) => Handle m -> Request -> ExceptT ReqError m ()
tokenAdminAuth h req = hideErr $ do
  Token tokenParam <- parseQueryStr req
  lift $ logInfo (hLog h) "Token parsed"
  checkAdminTokenParam h tokenParam

checkAdminTokenParam :: (MonadCatch m) => Handle m -> Text -> ExceptT ReqError m ()  
checkAdminTokenParam h tokenParam = hideTokenErr $ do
  case break (== '.') . unpack $ tokenParam of
    (usIdParam, _:xs) -> case break (== '.') xs of
      (tokenKeyParam, '.':'h':'i':'j':'.':ys) -> do
        usIdNum <- tryReadNum (pack usIdParam)
        maybeTokenKey <- checkMaybeOneE (hLog h) $ selectTxts h "users" ["token_key"] "user_id=?" [pack usIdParam] 
        case maybeTokenKey of
          Just tokenKey ->  
            if strSha1 (unpack tokenKey) == tokenKeyParam 
                 && strSha1 ("hij" ++ unpack tokenKey) == ys
              then do
                lift $ logInfo (hLog h) $ "Token valid, user in AdminAccessMode. Admin_id: " ++ show usIdNum
                return ()
              else throwE . SimpleError $ "INVALID token"
          Nothing -> throwE . SimpleError $ "INVALID token"
      _ -> throwE . SimpleError $ "INVALID token"
    _        -> throwE . SimpleError $ "INVALID token"

tokenUserAuth :: (MonadCatch m) => Handle m -> Request -> ExceptT ReqError m UserAccessMode
tokenUserAuth h req = hideTokenErr $ do
  Token tokenParam <- parseQueryStr req
  lift $ logInfo (hLog h) "Token parsed"
  checkUserTokenParam h tokenParam

checkUserTokenParam :: (MonadCatch m) => Handle m -> Text -> ExceptT ReqError m UserAccessMode    
checkUserTokenParam h tokenParam = hideTokenErr $ do
  case break (== '.') . unpack $ tokenParam of
    (usIdParam, _:xs) -> case break (== '.') xs of
      (tokenKeyParam, '.':'s':'t':'u':'.':ys) -> do
        usIdNum <- tryReadNum (pack usIdParam)
        maybeTokenKey <- checkMaybeOneE (hLog h) $ selectTxts h "users" ["token_key"] "user_id=?" [pack usIdParam] 
        case maybeTokenKey of
          Just tokenKey ->  
            if strSha1 (unpack tokenKey) == tokenKeyParam 
                 && strSha1 ("stu" ++ unpack tokenKey) == ys 
              then do
                lift $ logInfo (hLog h) "Token valid, user in UserAccessMode"
                return (usIdNum, UserMode)
              else throwE . SimpleError $ "INVALID token"
          Nothing -> throwE . SimpleError $ "INVALID token"
      (tokenKeyParam, '.':'h':'i':'j':'.':ys) -> do
        usIdNum <- tryReadNum (pack usIdParam)
        maybeTokenKey <- checkMaybeOneE (hLog h) $ selectTxts h "users" ["token_key"] "user_id=?" [pack usIdParam] 
        case maybeTokenKey of
          Just tokenKey ->  
            if strSha1 (unpack tokenKey) == tokenKeyParam 
                 && strSha1 ("hij" ++ unpack tokenKey) == ys
              then do
                lift $ logInfo (hLog h) "Token valid, user in AdminAccessMode"
                return (usIdNum, AdminMode)
              else throwE . SimpleError $ "INVALID token"
          Nothing -> throwE . SimpleError $ "INVALID token"  
      _ -> throwE . SimpleError $ "INVALID token"
    _        -> throwE . SimpleError $ "INVALID token"

checkPwd :: (MonadCatch m) => Text -> Text -> ExceptT ReqError m ()
checkPwd pwdParam pwd 
  | pwd == hashPwdParam = return ()
  | otherwise       = throwE . SimpleError $ "INVALID password"
    where
      hashPwdParam = txtSha1 pwdParam

updateInDbE :: (MonadCatch m) => Handle m -> Table -> Set -> Where -> [Text] -> ExceptT ReqError m ()
updateInDbE h t s w values = checkUpdE (hLog h) $ updateInDb h t s w values