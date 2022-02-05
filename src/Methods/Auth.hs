--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}




module Methods.Auth where
          
import           Api.Response
import           Logger
import           Types
import           Oops
import           Methods.Handle
import ParseQueryStr (Token(..),LogIn(..),parseQueryStr)
import Conf (Config(..))
import           Network.Wai (Request,ResponseReceived,Response,responseBuilder,strictRequestBody,pathInfo)
import           Data.Text                      ( pack, unpack, Text )
import           Database.PostgreSQL.Simple (query, withTransaction, execute, executeMany,Connection,Only(..),Binary(Binary))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( catch, throwM, MonadCatch)


logIn :: (MonadCatch m) => MethodsHandle m -> LogIn -> ExceptT ReqError m ResponseInfo
logIn h (LogIn usIdNum pwdParam) = do
  let usIdParam = numToTxt usIdNum
  lift $ logInfo (hLog h) $ "All logIn parameters parsed"
  Auth pwd admBool <- selectOneIfExistE h "users" ["password","admin"] "user_id=?" usIdParam 
  case admBool of
    False -> do
      checkPwd pwdParam pwd
      tokenKey <- lift $ getTokenKey h
      updateInDbE h "users" "token_key=?" "user_id=?" [pack tokenKey,usIdParam]
      let usToken = pack $ show usIdNum ++ "." ++ strSha1 tokenKey ++ ".stu." ++ strSha1 ("stu" ++ tokenKey)
      okHelper $ TokenResponse {tokenTR = usToken}          
    True -> do 
      checkPwd pwdParam pwd
      tokenKey <- lift $ getTokenKey h
      updateInDbE h "users" "token_key=?" "user_id=?" [pack tokenKey,usIdParam]
      let usToken = pack $ show usIdNum ++ "." ++ strSha1 tokenKey ++ ".hij." ++ strSha1 ("hij" ++ tokenKey)
      okHelper $ TokenResponse {tokenTR = usToken}  
  
type UserAccessMode = (UserId,AccessMode)
data AccessMode = UserMode | AdminMode

tokenAdminAuth :: (MonadCatch m) => MethodsHandle m -> Request -> ExceptT ReqError m ()
tokenAdminAuth h req = do
  Token tokenParam <- parseQueryStr req
  lift $ logInfo (hLog h) $ "Token parsed"
  hideErr $ checkAdminTokenParam h tokenParam

checkAdminTokenParam :: (MonadCatch m) => MethodsHandle m -> Text -> ExceptT ReqError m ()  
checkAdminTokenParam h tokenParam =
  case break (== '.') . unpack $ tokenParam of
    (usIdParam, _:xs) -> case break (== '.') xs of
      (tokenKeyParam, '.':'h':'i':'j':'.':ys) -> do
        usIdNum <- tryReadNum (pack usIdParam)
        maybeTokenKey <- selectMaybeOneE h "users" ["token_key"] "user_id=?" [pack usIdParam] 
        case maybeTokenKey of
          Just (Only tokenKey) ->  
            if strSha1 (unpack tokenKey) == tokenKeyParam 
                 && strSha1 ("hij" ++ (unpack tokenKey)) == ys
              then do
                lift $ logInfo (hLog h) $ "Token valid, user in AdminAccessMode. Admin_id: " ++ show usIdNum
                return $ ()
              else throwE . SimpleError $ "INVALID token"
          Nothing -> throwE . SimpleError $ "INVALID token"
      _ -> throwE . SimpleError $ "INVALID token"
    _        -> throwE . SimpleError $ "INVALID token"

tokenUserAuth :: (MonadCatch m) => MethodsHandle m -> Request -> ExceptT ReqError m UserAccessMode
tokenUserAuth h req = do
  Token tokenParam <- parseQueryStr req
  lift $ logInfo (hLog h) $ "Token parsed"
  checkUserTokenParam h tokenParam

checkUserTokenParam :: (MonadCatch m) => MethodsHandle m -> Text -> ExceptT ReqError m UserAccessMode    
checkUserTokenParam h tokenParam =
  case break (== '.') . unpack $ tokenParam of
    (usIdParam, _:xs) -> case break (== '.') xs of
      (tokenKeyParam, '.':'s':'t':'u':'.':ys) -> do
        usIdNum <- tryReadNum (pack usIdParam)
        maybeTokenKey <- selectMaybeOneE h "users" ["token_key"] "user_id=?" [pack usIdParam] 
        case maybeTokenKey of
          Just (Only tokenKey) ->  
            if strSha1 (unpack tokenKey) == tokenKeyParam 
                 && strSha1 ("stu" ++ (unpack tokenKey)) == ys 
              then do
                lift $ logInfo (hLog h) $ "Token valid, user in UserAccessMode"
                return $ (usIdNum, UserMode)
              else throwE . SimpleError $ "INVALID token"
          Nothing -> throwE . SimpleError $ "INVALID token"
      (tokenKeyParam, '.':'h':'i':'j':'.':ys) -> do
        usIdNum <- tryReadNum (pack usIdParam)
        maybeTokenKey <- selectMaybeOneE h "users" ["token_key"] "user_id=?" [pack usIdParam] 
        case maybeTokenKey of
          Just (Only tokenKey) ->  
            if strSha1 (unpack tokenKey) == tokenKeyParam 
                 && strSha1 ("hij" ++ (unpack tokenKey)) == ys
              then do
                lift $ logInfo (hLog h) $ "Token valid, user in AdminAccessMode"
                return $ (usIdNum, AdminMode)
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