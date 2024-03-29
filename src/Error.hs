module Error
  ( MigrationException (..),
    ReqError (..),
    logOnErr,
    hideErr,
    hideLogInErr,
    hideTokenErr,
    catchDbErr,
    UnexpectedDbOutPutException (..),
    addToBadReqErr,
    hideResourceNotExistErr,
  )
where

import qualified Control.Exception as E
import Control.Monad.Catch (Exception, MonadCatch, catch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, throwE)
import Database.PostgreSQL.Simple (FormatError, QueryError, ResultError, SqlError)
import Logger (LogHandle (..), logWarning)

data ReqError
  = SecretError String
  | DatabaseError String
  | SecretLogInError String
  | SecretTokenError String
  | BadReqError String
  | ResourceNotExistError String
  | NotImplementedError String
  | UriTooLongError String
  | ForbiddenError String
  | ReqBodyTooLargeError String
  deriving (Eq, Show)

data UnexpectedDbOutPutException
  = UnexpectedEmptyDbOutPutException
  | UnexpectedMultipleDbOutPutException
  deriving (Eq, Show)

instance Exception UnexpectedDbOutPutException

newtype MigrationException = MigrationException String
  deriving (Eq, Show)

instance Exception MigrationException

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
hideLogInErr m = m `catchE` (throwE . badReqToSecretLogIn)

hideTokenErr :: (MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
hideTokenErr m = m `catchE` (throwE . badReqToSecretToken)

hideResourceNotExistErr :: (MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
hideResourceNotExistErr m = m `catchE` (throwE . badReqToResourceNotExistError)

addToBadReqErr :: (Monad m) => String -> ReqError -> ExceptT ReqError m a
addToBadReqErr addStr (BadReqError str) = throwE $ BadReqError $ str ++ addStr
addToBadReqErr _ e = throwE e

catchDbErr :: (MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
catchDbErr =
  catchDbOutputErr . catchIOErr . cathResultErr . cathQueryErr . cathFormatErr . cathSqlErr

cathSqlErr,
  cathFormatErr,
  cathQueryErr,
  cathResultErr,
  catchIOErr,
  catchDbOutputErr ::
    (MonadCatch m) => ExceptT ReqError m a -> ExceptT ReqError m a
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
toSecret e = SecretError (show e)

badReqToSecretLogIn :: ReqError -> ReqError
badReqToSecretLogIn e@(BadReqError _) = SecretLogInError $ show e
badReqToSecretLogIn e = e

badReqToSecretToken :: ReqError -> ReqError
badReqToSecretToken e@(BadReqError _) = SecretTokenError $ show e
badReqToSecretToken e = e

badReqToResourceNotExistError :: ReqError -> ReqError
badReqToResourceNotExistError e@(BadReqError _) = ResourceNotExistError $ show e
badReqToResourceNotExistError e = e
