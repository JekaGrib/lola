{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Common.Auth where

import Api.Request.QueryStr (Token (..), parseQueryStr)
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Text (Text, pack, unpack)
import Logger
import Methods.Common
import Network.HTTP.Types.URI (QueryText)
import Error (ReqError (..), hideErr, hideTokenErr)
import Psql.Methods.Common.Auth
import TryRead (tryReadId)
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectTokenKeysForUser :: UserId -> m [TokenKey]
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectTokenKeysForUser' conn)

type UserAccessMode = (UserId, AccessMode)

data AccessMode = UserMode | AdminMode
  deriving (Eq, Show)

tokenAdminAuth :: (MonadCatch m) => Handle m -> QueryText -> ExceptT ReqError m ()
tokenAdminAuth h qStr = hideErr $ do
  Token tokenParam <- parseQueryStr qStr
  lift $ logInfo (hLog h) "Token parsed"
  checkAdminTokenParam h tokenParam

checkAdminTokenParam :: (MonadCatch m) => Handle m -> Text -> ExceptT ReqError m ()
checkAdminTokenParam Handle {..} tokenParam =
  case break (== '.') . unpack $ tokenParam of
    (usIdParam, '.' : 'h' : 'i' : 'j' : '.' : xs) -> do
      usIdNum <- tryReadId "user_id" (pack usIdParam)
      maybeTokenKey <- catchMaybeOneSelE hLog $ selectTokenKeysForUser usIdNum
      case maybeTokenKey of
        Just tokenKey ->
          if strSha1 ("hij" ++ tokenKey) == xs
            then do
              lift $ logInfo hLog $ "Token valid, user in AdminAccessMode. Admin_id: " ++ show usIdNum
              return ()
            else throwE . SecretTokenError $ "INVALID token. Wrong token key or user_id"
        Nothing -> throwE . SecretTokenError $ "INVALID token. User doesn`t exist"
    _ -> throwE . SecretTokenError $ "INVALID token"

tokenUserAuth :: (MonadCatch m) => Handle m -> QueryText -> ExceptT ReqError m UserAccessMode
tokenUserAuth h qStr = hideTokenErr $ do
  Token tokenParam <- parseQueryStr qStr
  lift $ logInfo (hLog h) "Token parsed"
  checkUserTokenParam h tokenParam

checkUserTokenParam :: (MonadCatch m) => Handle m -> Text -> ExceptT ReqError m UserAccessMode
checkUserTokenParam Handle {..} tokenParam =
  case break (== '.') . unpack $ tokenParam of
    (usIdParam, '.' : 'h' : 'i' : 'j' : '.' : xs) -> do
      usIdNum <- tryReadId "user_id" (pack usIdParam)
      maybeTokenKey <- catchMaybeOneSelE hLog $ selectTokenKeysForUser usIdNum
      case maybeTokenKey of
        Just tokenKey ->
          if strSha1 ("hij" ++ tokenKey) == xs
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
          if strSha1 ("stu" ++ tokenKey) == xs
            then do
              lift $ logInfo hLog $ "Token valid, user in UserAccessMode. User_id: " ++ show usIdNum
              return (usIdNum, UserMode)
            else throwE . SecretTokenError $ "INVALID token. Wrong token key or user_id"
        Nothing -> throwE . SecretTokenError $ "INVALID token. User doesn`t exist"
    _ -> throwE . SecretTokenError $ "INVALID token"
