{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}




module Oops where

import Logger (LogHandle(..), logWarning)
import qualified Control.Exception              as E
import           Database.PostgreSQL.Simple (SqlError,FormatError,QueryError,ResultError)
import           Control.Monad.Trans.Except (ExceptT,catchE,throwE)
import           Control.Monad.Catch            ( catch, MonadCatch)
import           Control.Monad.Trans            ( lift )


data ReqError = SecretError String | SimpleError String | DatabaseError String | DatabaseAndUnrollError String
  deriving (Eq,Show)


logOnErr :: (Monad m, MonadCatch m,MonadFail m) => LogHandle m  -> ExceptT ReqError m a -> ExceptT ReqError m a
logOnErr logH m = m `catchE` (\e -> do
  lift $ logWarning logH $ show e
  throwE e)


hideErr :: (Monad m, MonadCatch m, MonadFail m) => ExceptT ReqError m a -> ExceptT ReqError m a
hideErr m = m `catchE` (\e -> throwE $ toSecret e)


catchDbErr ::  (Monad m, MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
catchDbErr m = catchIOErr . cathResultErr . cathQueryErr . cathFormatErr . cathSqlErr $ m 

cathSqlErr,cathFormatErr,cathQueryErr,cathResultErr, catchIOErr :: (Monad m, MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
cathSqlErr m = m `catch` (\e -> do
  throwE . DatabaseError $ show (e :: SqlError) )

cathFormatErr m = m `catch` (\e -> do
  throwE . DatabaseError $ show (e :: FormatError) )

cathQueryErr m = m `catch` (\e -> do
  throwE . DatabaseError $ show (e :: QueryError) )

cathResultErr m = m `catch` (\e -> do
  throwE . DatabaseError $ show (e :: ResultError) )

catchIOErr m = m `catch` (\e -> do
  throwE . DatabaseError $ show (e :: E.IOException) )


toSecret :: ReqError -> ReqError
toSecret (SimpleError str) = SecretError str
toSecret (SecretError str) = SecretError str
toSecret (DatabaseError str) = SecretError str
toSecret (DatabaseAndUnrollError str) = SecretError str



