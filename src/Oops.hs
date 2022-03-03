{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Oops (ReqError (..), logOnErr, hideErr, hideLogInErr, hideTokenErr, catchDbErr, UnexpectedDbOutPutException (..), addToSimpleErr) where

import qualified Control.Exception as E
import Control.Monad.Catch (Exception, MonadCatch, catch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, throwE)
import Database.PostgreSQL.Simple (FormatError, QueryError, ResultError, SqlError)
import Logger (LogHandle (..), logWarning)

data ReqError = 
  SecretError String 
  | SimpleError String 
  | DatabaseError String 
  | SecretLogInError String 
  | SecretTokenError String 
  | Error400 String
  | Error404 String
  deriving (Eq, Show)

data UnexpectedDbOutPutException = UnexpectedEmptyDbOutPutException | UnexpectedMultipleDbOutPutException
  deriving (Eq, Show)

instance Exception UnexpectedDbOutPutException

logOnErr :: (MonadCatch m) => LogHandle m -> ExceptT ReqError m a -> ExceptT ReqError m a
logOnErr logH m =
  m
    `catchE` ( \e -> do
                 lift $ logWarning logH $ show e
                 throwE e
             )

hideErr :: (MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
hideErr m = m `catchE` (throwE . toSecret)

hideLogInErr :: (MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
hideLogInErr m = m `catchE` (throwE . simpleToSecretLogIn)

hideTokenErr :: (MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
hideTokenErr m = m `catchE` (throwE . simpleToSecretToken)

addToSimpleErr :: (Monad m) => String -> ReqError -> ExceptT ReqError m a
addToSimpleErr str2 (SimpleError str1) = throwE $ SimpleError $ str1 ++ str2
addToSimpleErr _ e = throwE e

catchDbErr :: (MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
catchDbErr = catchDbOutputErr . catchIOErr . cathResultErr . cathQueryErr . cathFormatErr . cathSqlErr

cathSqlErr, cathFormatErr, cathQueryErr, cathResultErr, catchIOErr, catchDbOutputErr :: (MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
cathSqlErr m =
  m
    `catch` ( \e ->
                throwE . DatabaseError $ show (e :: SqlError)
            )
cathFormatErr m =
  m
    `catch` ( \e ->
                throwE . DatabaseError $ show (e :: FormatError)
            )
cathQueryErr m =
  m
    `catch` ( \e ->
                throwE . DatabaseError $ show (e :: QueryError)
            )
cathResultErr m =
  m
    `catch` ( \e ->
                throwE . DatabaseError $ show (e :: ResultError)
            )
catchIOErr m =
  m
    `catch` ( \e ->
                throwE . DatabaseError $ show (e :: E.IOException)
            )
catchDbOutputErr m =
  m
    `catch` ( \e ->
                throwE . DatabaseError $ show (e :: UnexpectedDbOutPutException)
            )

toSecret :: ReqError -> ReqError
toSecret (SimpleError str) = SecretError str
toSecret (SecretError str) = SecretError str
toSecret (DatabaseError str) = SecretError ("DatabaseError. " ++ str)
toSecret (SecretLogInError str) = SecretError ("LogInError. " ++ str)
toSecret (SecretTokenError str) = SecretError ("TokenError. " ++ str)

simpleToSecretLogIn :: ReqError -> ReqError
simpleToSecretLogIn (SimpleError str) = SecretLogInError str
simpleToSecretLogIn e = e

simpleToSecretToken :: ReqError -> ReqError
simpleToSecretToken (SimpleError str) = SecretTokenError str
simpleToSecretToken e = e
