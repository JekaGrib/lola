{-# LANGUAGE RankNTypes #-}

module Methods.Author where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateAuthor (..), UpdateAuthor (..), checkQStr)
import Api.Response (AuthorResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Database.PostgreSQL.Simple (withTransaction)
import Error (ReqError (..), catchDbErr)
import Logger
import Methods.Common
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth)
import Methods.Common.DeleteMany (deleteAllAboutDrafts)
import qualified Methods.Common.DeleteMany (Handle, makeH)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourceE)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Network.HTTP.Types (QueryText)
import Psql.Methods.Author
import Psql.Selecty (Author (..))
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectDraftsForAuthor :: AuthorId -> m [DraftId],
    selectAuthorsForUser :: UserId -> m [AuthorId],
    selectAuthors :: AuthorId -> m [Author],
    updateDbAuthor :: UserId -> AuthorInfo -> AuthorId -> m (),
    updateDbAuthorForPosts :: AuthorId -> AuthorId -> m (),
    deleteDbAuthor :: AuthorId -> m (),
    isUserAuthor :: UserId -> m Bool,
    insertReturnAuthor :: UserId -> AuthorInfo -> m Id,
    withTransactionDB :: forall a. m a -> m a,
    hDelMany :: Methods.Common.DeleteMany.Handle m,
    hAuth :: Methods.Common.Auth.Handle m,
    hExist :: Methods.Common.Exist.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectDraftsForAuthor' conn)
        (selectAuthorsForUser' conn)
        (selectAuthors' conn)
        (updateDbAuthor' conn)
        (updateDbAuthorForPosts' conn)
        (deleteDbAuthor' conn)
        (isUserAuthor' conn)
        (insertReturnAuthor' conn)
        (withTransaction conn)
        (Methods.Common.DeleteMany.makeH conf)
        (Methods.Common.Auth.makeH conf logH)
        (Methods.Common.Exist.makeH conf)

workWithAuthors ::
  (MonadCatch m) =>
  Handle m ->
  QueryText ->
  AppMethod ->
  ExceptT ReqError m ResponseInfo
workWithAuthors h@Handle {..} qStr meth =
  case meth of
    ToPost -> do
      lift $ logInfo hLog "Create author command"
      tokenAdminAuth hAuth qStr
      checkQStr hExist qStr >>= createAuthor h
    ToGet auId -> do
      lift $ logInfo hLog "Get author command"
      tokenAdminAuth hAuth qStr
      isExistResourceE hExist (AuthorId auId)
      getAuthor h auId
    ToPut auId -> do
      lift $ logInfo hLog "Update author command"
      tokenAdminAuth hAuth qStr
      isExistResourceE hExist (AuthorId auId)
      checkQStr hExist qStr >>= updateAuthor h auId
    ToDelete auId -> do
      lift $ logInfo hLog "Delete author command"
      tokenAdminAuth hAuth qStr
      isExistResourceE hExist (AuthorId auId)
      deleteAuthor h auId
    _ ->
      throwE $ ResourceNotExistError $
        "Wrong method for authors resource: " ++ show meth

createAuthor ::
  (MonadCatch m) =>
  Handle m ->
  CreateAuthor ->
  ExceptT ReqError m ResponseInfo
createAuthor h@Handle {..} (CreateAuthor usIdParam auInfoParam) = do
  isNotAlreadyAuthor h usIdParam
  auId <- catchInsertReturnE hLog $ insertReturnAuthor usIdParam auInfoParam
  lift $ logInfo hLog $ "Author_id: " ++ show auId ++ " created"
  ok201Helper hConf "author" auId

getAuthor :: (MonadCatch m) => Handle m -> AuthorId -> ExceptT ReqError m ResponseInfo
getAuthor Handle {..} authId = do
  Author auId auInfo usId <- catchOneSelectE hLog $ selectAuthors authId
  lift $ logInfo hLog $ "Author_id: " ++ show auId ++ " sending in response."
  okHelper $ AuthorResponse {authorIdA = auId, userIdA = usId, authorInfoA = auInfo}

updateAuthor ::
  (MonadCatch m) =>
  Handle m ->
  AuthorId ->
  UpdateAuthor ->
  ExceptT ReqError m ResponseInfo
updateAuthor h@Handle {..} auId (UpdateAuthor usIdParam auInfoParam) = do
  isntUserOtherAuthor h usIdParam auId
  catchUpdE hLog $ updateDbAuthor usIdParam auInfoParam auId
  lift $ logInfo hLog $ "Author_id: " ++ show auId ++ " updated."
  okHelper $
    AuthorResponse {authorIdA = auId, userIdA = usIdParam, authorInfoA = auInfoParam}

deleteAuthor :: (MonadCatch m) => Handle m -> AuthorId -> ExceptT ReqError m ResponseInfo
deleteAuthor h@Handle {..} auId = do
  let updatePos = updateDbAuthorForPosts (cDefAuthId hConf) auId
  draftsIds <- catchSelE hLog $ selectDraftsForAuthor auId
  let deleteDr = deleteAllAboutDrafts hDelMany draftsIds
  let deleteAu = deleteDbAuthor auId
  withTransactionDBE h (updatePos >> deleteDr >> deleteAu)
  lift $ logInfo hLog $ "Author_id: " ++ show auId ++ " deleted."
  ok204Helper

isntUserOtherAuthor ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  AuthorId ->
  ExceptT ReqError m ()
isntUserOtherAuthor Handle {..} usId auId = do
  maybeAuId <- catchMaybeOneSelectE hLog $ selectAuthorsForUser usId
  case maybeAuId of
    Just usAuId ->
      if usAuId == auId
        then return ()
        else throwE $ BadReqError $ "User_id: " ++ show usId ++ " is already other author"
    Nothing -> return ()

isNotAlreadyAuthor :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m ()
isNotAlreadyAuthor Handle {..} usId = do
  lift $ logDebug hLog "Checking is user already in authors in DB"
  isEx <- catchDbErr $ lift $ isUserAuthor usId
  when isEx
    $ throwE
    $ BadReqError "User is already author."

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchTransactionE (hLog h) . withTransactionDB h
