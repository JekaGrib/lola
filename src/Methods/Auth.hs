{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Methods.Common.Selecty (Auth (..))
import Network.Wai (Request)
import Oops
import ParseQueryStr (LogIn (..), Token (..), parseQueryStr)
import TryRead (tryReadId)
import Types

data Handle m = Handle
  { hConf :: Config
  , hLog :: LogHandle m
  , selectTokenKeysForUser :: UserId -> m [TokenKey]
  , selectAuthsForUser :: UserId -> m [Auth]
  , updateDbTokenKeyForUser :: TokenKey -> UserId -> m ()
  , getTokenKey :: m String
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

selectTokenKeysForUser' conn usId = 
  let wh = WherePair "user_id=?" (Id usId)
  selectOnly' conn $ Select ["token_key"] "users" wh
selectAuthsForUser' conn usId = 
  let wh = WherePair "user_id=?" (Id usId)
  selectOnly' conn $ Select ["password", "admin"] "users" wh
updateDbTokenKeyForUser' conn tokenKey usId = do
  let set = SetPair "token_key=?" (Str tokenKey)
  let wh = WherePair "user_id=?" (Id usId)
  updateInDb' conn (Update "users" set wh)

logIn :: (MonadCatch m) => Handle m -> LogIn -> ExceptT ReqError m ResponseInfo
logIn Handle{..} (LogIn usIdParam pwdParam) = do
  let logpair = ("user_id",usIdParam)
  Auth pwd admBool <- checkOneIfExistE hLog logpair $ selectAuthsForUser usIdParam
  checkPwd pwdParam pwd
  tokenKey <- lift $ getTokenKey 
  catchUpdE hLog $ updateDbTokenKeyForUser tokenKey usIdParam
  if admBool
    then do
      let usToken = pack $ show usIdParam ++ ".hij." ++ strSha1 ("hij" ++ tokenKey)
      lift $ logInfo hLog $ "User_id: " ++ show usIdParam ++ " successfully logIn as admin."
      okHelper $ TokenResponse {tokenTR = usToken}
    else do
      let usToken = pack $ show usIdParam ++ ".stu." ++ strSha1 ("stu" ++ tokenKey)
      lift $ logInfo hLog $ "User_id: " ++ show usIdParam ++ " successfully logIn as user."
      okHelper $ TokenResponse {tokenTR = usToken}

type UserAccessMode = (UserId, AccessMode)

data AccessMode = UserMode | AdminMode

tokenAdminAuth :: (MonadCatch m) => Handle m -> Request -> ExceptT ReqError m ()
tokenAdminAuth h req = hideErr $ do
  Token tokenParam <- parseQueryStr req
  lift $ logInfo (hLog h) "Token parsed"
  checkAdminTokenParam h tokenParam

checkAdminTokenParam :: (MonadCatch m) => Handle m -> Text -> ExceptT ReqError m ()
checkAdminTokenParam Handle{..} tokenParam = hideTokenErr $
  case break (== '.') . unpack $ tokenParam of
    (usIdParam, '.' : 'h' : 'i' : 'j' : '.' : xs) -> do
      usIdNum <- tryReadId "user_id" (pack usIdParam)
      maybeTokenKey <- catchMaybeOneSelE hLog $ selectTokenKeysForUser usIdNum
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
checkUserTokenParam Handle{..}  tokenParam = hideTokenErr $
  case break (== '.') . unpack $ tokenParam of
    (usIdParam, '.' : 'h' : 'i' : 'j' : '.' : xs) -> do
      usIdNum <- tryReadId "user_id" (pack usIdParam)
      maybeTokenKey <- catchMaybeOneSelE hLog $ selectTokenKeysForUser usIdNum
      case maybeTokenKey of
        Just tokenKey ->
          if strSha1 ("hij" ++ unpack tokenKey) == xs
            then do
              lift $ logInfo hLog $ "Token valid, user in AdminAccessMode. Admin_id: " ++ show usIdNum
              return (usIdNum, AdminMode)
            else throwE . SecretTokenError $ "INVALID token. Wrong token key or user_id"
        Nothing -> throwE . SecretTokenError $ "INVALID token. User doesn`t exist"
    (usIdParam, '.' : 's' : 't' : 'u' : '.' : xs) -> do
      usIdNum <- tryReadId "user_id" (pack usIdParam)
      maybeTokenKey <- catchMaybeOneSelE hLog $ selectTokenKeysForUser usIdNum
      case maybeTokenKey of
        Just tokenKey ->
          if strSha1 ("stu" ++ unpack tokenKey) == xs
            then do
              lift $ logInfo hLog $ "Token valid, user in UserAccessMode. User_id: " ++ show usIdNum
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

