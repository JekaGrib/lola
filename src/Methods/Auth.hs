{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Auth where

import Api.Response (TokenResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Text (Text, pack, unpack)
import Logger
import Methods.Common
import Methods.Common.Select (Auth (..))
import Network.Wai (Request)
import Oops
import ParseQueryStr (LogIn (..), Token (..), parseQueryStr)
import TryRead (tryReadId)
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectTxts :: Table -> [DbSelectParamKey] -> Where -> [DbParamValue] -> m [Text],
    selectAuths :: Table -> [DbSelectParamKey] -> Where -> [DbParamValue] -> m [Auth],
    updateInDb :: Table -> ToUpdate -> Where -> [DbParamValue] -> m (),
    getTokenKey :: m String
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
        getTokenKey'

logIn :: (MonadCatch m) => Handle m -> LogIn -> ExceptT ReqError m ResponseInfo
logIn h (LogIn usIdNum pwdParam) = do
  let usIdParam = numToTxt usIdNum
  Auth pwd admBool <- checkOneIfExistE (hLog h) (selectAuths h) "users" ["password", "admin"] "user_id=?" usIdParam
  checkPwd pwdParam pwd
  tokenKey <- lift $ getTokenKey h
  updateInDbE h "users" "token_key=?" "user_id=?" [pack tokenKey, usIdParam]
  if admBool
    then do
      let usToken = pack $ show usIdNum ++ ".hij." ++ strSha1 ("hij" ++ tokenKey)
      lift $ logInfo (hLog h) $ "User_id: " ++ show usIdNum ++ " successfully logIn as admin."
      okHelper $ TokenResponse {tokenTR = usToken}
    else do
      let usToken = pack $ show usIdNum ++ ".stu." ++ strSha1 ("stu" ++ tokenKey)
      lift $ logInfo (hLog h) $ "User_id: " ++ show usIdNum ++ " successfully logIn as user."
      okHelper $ TokenResponse {tokenTR = usToken}

type UserAccessMode = (UserId, AccessMode)

data AccessMode = UserMode | AdminMode

tokenAdminAuth :: (MonadCatch m) => Handle m -> Request -> ExceptT ReqError m ()
tokenAdminAuth h req = hideErr $ do
  Token tokenParam <- parseQueryStr req
  lift $ logInfo (hLog h) "Token parsed"
  checkAdminTokenParam h tokenParam

checkAdminTokenParam :: (MonadCatch m) => Handle m -> Text -> ExceptT ReqError m ()
checkAdminTokenParam h tokenParam = hideTokenErr $
  case break (== '.') . unpack $ tokenParam of
    (usIdParam, '.' : 'h' : 'i' : 'j' : '.' : xs) -> do
      usIdNum <- tryReadId "user_id" (pack usIdParam)
      maybeTokenKey <- checkMaybeOneE (hLog h) $ selectTxts h "users" ["token_key"] "user_id=?" [pack usIdParam]
      case maybeTokenKey of
        Just tokenKey ->
          if strSha1 ("hij" ++ unpack tokenKey) == xs
            then do
              lift $ logInfo (hLog h) $ "Token valid, user in AdminAccessMode. Admin_id: " ++ show usIdNum
              return ()
            else throwE . SecretTokenError $ "INVALID token. Wrong token key or user_id"
        Nothing -> throwE . SecretTokenError $ "INVALID token. User doesn`t exist"
    _ -> throwE . SecretTokenError $ "INVALID token"

tokenUserAuth :: (MonadCatch m) => Handle m -> Request -> ExceptT ReqError m UserAccessMode
tokenUserAuth h req = hideTokenErr $ do
  Token tokenParam <- parseQueryStr req
  lift $ logInfo (hLog h) "Token parsed"
  checkUserTokenParam h tokenParam

checkUserTokenParam :: (MonadCatch m) => Handle m -> Text -> ExceptT ReqError m UserAccessMode
checkUserTokenParam h tokenParam = hideTokenErr $
  case break (== '.') . unpack $ tokenParam of
    (usIdParam, '.' : 'h' : 'i' : 'j' : '.' : xs) -> do
      usIdNum <- tryReadId "user_id" (pack usIdParam)
      maybeTokenKey <- checkMaybeOneE (hLog h) $ selectTxts h "users" ["token_key"] "user_id=?" [pack usIdParam]
      case maybeTokenKey of
        Just tokenKey ->
          if strSha1 ("hij" ++ unpack tokenKey) == xs
            then do
              lift $ logInfo (hLog h) $ "Token valid, user in AdminAccessMode. Admin_id: " ++ show usIdNum
              return (usIdNum, AdminMode)
            else throwE . SecretTokenError $ "INVALID token. Wrong token key or user_id"
        Nothing -> throwE . SecretTokenError $ "INVALID token. User doesn`t exist"
    (usIdParam, '.' : 's' : 't' : 'u' : '.' : xs) -> do
      usIdNum <- tryReadId "user_id" (pack usIdParam)
      maybeTokenKey <- checkMaybeOneE (hLog h) $ selectTxts h "users" ["token_key"] "user_id=?" [pack usIdParam]
      case maybeTokenKey of
        Just tokenKey ->
          if strSha1 ("stu" ++ unpack tokenKey) == xs
            then do
              lift $ logInfo (hLog h) $ "Token valid, user in UserAccessMode. User_id: " ++ show usIdNum
              return (usIdNum, UserMode)
            else throwE . SecretTokenError $ "INVALID token. Wrong token key or user_id"
        Nothing -> throwE . SecretTokenError $ "INVALID token. User doesn`t exist"
    _ -> throwE . SecretTokenError $ "INVALID token"

checkPwd :: (MonadCatch m) => Text -> Text -> ExceptT ReqError m ()
checkPwd pwdParam pwd
  | pwd == hashPwdParam = return ()
  | otherwise = throwE . SecretLogInError $ "INVALID password"
  where
    hashPwdParam = txtSha1 pwdParam

updateInDbE :: (MonadCatch m) => Handle m -> Table -> Set -> Where -> [Text] -> ExceptT ReqError m ()
updateInDbE h t s w values = checkUpdE (hLog h) $ updateInDb h t s w values
