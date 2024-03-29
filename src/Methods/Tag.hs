{-# LANGUAGE RankNTypes #-}

module Methods.Tag where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateTag (..), UpdateTag (..), checkQStr)
import Api.Response (TagResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Database.PostgreSQL.Simple (withTransaction)
import Error (ReqError (..))
import Logger
import Methods.Common
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourceE)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Network.HTTP.Types (QueryText)
import Psql.Methods.Tag
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectTagNames :: TagId -> m [TagName],
    updateDbTag :: TagName -> TagId -> m (),
    deleteDbTag :: TagId -> m (),
    deleteDbTagForDrafts :: TagId -> m (),
    deleteDbTagForPosts :: TagId -> m (),
    insertReturnTag :: TagName -> m TagId,
    withTransactionDB :: forall a. m a -> m a,
    hAuth :: Methods.Common.Auth.Handle m,
    hExist :: Methods.Common.Exist.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectTagNames' conn)
        (updateDbTag' conn)
        (deleteDbTag' conn)
        (deleteDbTagForDrafts' conn)
        (deleteDbTagForPosts' conn)
        (insertReturnTag' conn)
        (withTransaction conn)
        (Methods.Common.Auth.makeH conf logH)
        (Methods.Common.Exist.makeH conf)

workWithTags ::
  (MonadCatch m) =>
  Handle m ->
  QueryText ->
  AppMethod ->
  ExceptT ReqError m ResponseInfo
workWithTags h@Handle {..} qStr meth =
  case meth of
    ToPost -> do
      lift $ logInfo hLog "Create tag command"
      tokenAdminAuth hAuth qStr
      checkQStr hExist qStr >>= createTag h
    ToGet tagId -> do
      lift $ logInfo hLog "Get tag command"
      isExistResourceE hExist (TagId tagId)
      getTag h tagId
    ToPut tagId -> do
      lift $ logInfo hLog "Update tag command"
      tokenAdminAuth hAuth qStr
      isExistResourceE hExist (TagId tagId)
      checkQStr hExist qStr >>= updateTag h tagId
    ToDelete tagId -> do
      lift $ logInfo hLog "Delete tag command"
      tokenAdminAuth hAuth qStr
      isExistResourceE hExist (TagId tagId)
      deleteTag h tagId
    _ ->
      throwE $ ResourceNotExistError $
        "Wrong method for tags resource: " ++ show meth

createTag ::
  (Monad m, MonadCatch m) =>
  Handle m ->
  CreateTag ->
  ExceptT ReqError m ResponseInfo
createTag Handle {..} (CreateTag tagNameParam) = do
  tagId <- catchInsertReturnE hLog $ insertReturnTag tagNameParam
  lift $ logInfo hLog $ "Tag_id: " ++ show tagId ++ " created"
  ok201Helper hConf "tag" tagId

getTag :: (Monad m, MonadCatch m) => Handle m -> TagId -> ExceptT ReqError m ResponseInfo
getTag Handle {..} tagId = do
  tagName <- catchOneSelectE hLog $ selectTagNames tagId
  lift $ logInfo hLog $ "Tag_id: " ++ show tagId ++ " sending in response"
  okHelper $ TagResponse tagId tagName

updateTag ::
  (Monad m, MonadCatch m) =>
  Handle m ->
  TagId ->
  UpdateTag ->
  ExceptT ReqError m ResponseInfo
updateTag Handle {..} tagId (UpdateTag tagNameParam) = do
  catchUpdE hLog $ updateDbTag tagNameParam tagId
  lift $ logInfo hLog $ "Tag_id: " ++ show tagId ++ " updated"
  okHelper $ TagResponse tagId tagNameParam

deleteTag :: (Monad m, MonadCatch m) => Handle m -> TagId -> ExceptT ReqError m ResponseInfo
deleteTag h@Handle {..} tagId = do
  let deleteTgDr = deleteDbTagForDrafts tagId
  let deleteTgPos = deleteDbTagForPosts tagId
  let deleteTg = deleteDbTag tagId
  withTransactionDBE h (deleteTgDr >> deleteTgPos >> deleteTg)
  lift $ logInfo hLog $ "Tag_id: " ++ show tagId ++ " deleted"
  ok204Helper

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchTransactionE (hLog h) . withTransactionDB h
