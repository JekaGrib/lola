{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}




module Oops (ReqError(..),logOnErr,hideErr,hideLogInErr,hideTokenErr,catchDbErr,UnexpectedDbOutPutException(..)) where

import Logger (LogHandle(..), logWarning)
import qualified Control.Exception              as E
import           Database.PostgreSQL.Simple (SqlError,FormatError,QueryError,ResultError)
import           Control.Monad.Trans.Except (ExceptT,catchE,throwE)
import           Control.Monad.Catch            ( catch, MonadCatch,Exception)
import           Control.Monad.Trans            ( lift )


data ReqError = SecretError String | SimpleError String | DatabaseError String | SecretLogInError String | SecretTokenError String
  deriving (Eq,Show)

data UnexpectedDbOutPutException = UnexpectedEmptyDbOutPutException | UnexpectedMultipleDbOutPutException
  deriving (Eq,Show)
  

instance Exception UnexpectedDbOutPutException

logOnErr :: (Monad m, MonadCatch m) => LogHandle m  -> ExceptT ReqError m a -> ExceptT ReqError m a
logOnErr logH m = m `catchE` (\e -> do
  lift $ logWarning logH $ show e
  throwE e)


hideErr :: (Monad m, MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
hideErr m = m `catchE` (throwE . toSecret)

hideLogInErr :: (Monad m, MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
hideLogInErr m = m `catchE` (throwE . simpleToSecretLogIn)

hideTokenErr :: (Monad m, MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
hideTokenErr m = m `catchE` (throwE . simpleToSecretToken)

catchDbErr ::  (Monad m, MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
catchDbErr = catchDbOutputErr . catchIOErr . cathResultErr . cathQueryErr . cathFormatErr . cathSqlErr  

cathSqlErr,cathFormatErr,cathQueryErr,cathResultErr, catchIOErr, catchDbOutputErr :: (Monad m, MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
cathSqlErr m = m `catch` (\e -> 
  throwE . DatabaseError $ show (e :: SqlError) )

cathFormatErr m = m `catch` (\e -> 
  throwE . DatabaseError $ show (e :: FormatError) )

cathQueryErr m = m `catch` (\e -> 
  throwE . DatabaseError $ show (e :: QueryError) )

cathResultErr m = m `catch` (\e -> 
  throwE . DatabaseError $ show (e :: ResultError) )

catchIOErr m = m `catch` (\e -> 
  throwE . DatabaseError $ show (e :: E.IOException) )

catchDbOutputErr m = m `catch` (\e -> 
  throwE . DatabaseError $ show (e :: UnexpectedDbOutPutException) )

toSecret :: ReqError -> ReqError
toSecret (SimpleError str) = SecretError str
toSecret (SecretError str) = SecretError str
toSecret (DatabaseError str) = SecretError str
toSecret (SecretLogInError str) = SecretError str
toSecret (SecretTokenError str) = SecretError str


simpleToSecretLogIn :: ReqError -> ReqError
simpleToSecretLogIn (SimpleError str) = SecretLogInError str
simpleToSecretLogIn e = e

simpleToSecretToken :: ReqError -> ReqError
simpleToSecretToken (SimpleError str) = SecretTokenError str
simpleToSecretToken e = e
