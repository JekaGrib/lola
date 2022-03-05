


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Tag where

import Api.Response (OkResponse (..), TagResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import Database.PostgreSQL.Simple (withTransaction)
import Logger
import Methods.Common
import Oops
import ParseQueryStr (CreateTag (..), DeleteTag (..), UpdateTag (..))
import Types

data Handle m = Handle
  { hConf                :: Config
  , hLog                 :: LogHandle m
  , selectTagNames       :: TagId -> m [TagName]
  , updateDbTagName      :: TagName -> TagId -> m ()
  , deleteDbTag          :: TagId -> m ()
  , deleteDbTagForDrafts :: TagId -> m ()
  , deleteDbTagForPosts  :: TagId -> m ()
  , isExistTag           :: TagId -> m Bool
  , insertReturnTag      :: TagName -> m TagId
  , withTransactionDB    :: forall a. m a -> m a
  , hAuth                :: Methods.Auth.Handle
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
        (isExistTag' conn)
        (insertReturnTag' conn)
        (withTransaction conn)

selectTagNames' conn tagId = do
  let wh = WherePair "tag_id=?" (Id tagId)
  selectOnly' conn (Select ["tag_name"] "tags" wh)
updateDbTag' conn tagName tagId = do
  let set = SetPair "tag_name=?" (Txt tagName)
  let wh = WherePair "tag_id=?" (Id tagId)
  updateInDb' conn (Update "tags" [set] wh)
deleteDbTag' conn tagId = do
  let wh = WherePair "tag_id=?" (Id tagId)
  deleteFromDb' conn (Delete "tags" wh)
deleteDbTagForPosts' conn tagId = do
  let wh = WherePair "tag_id=?" (Id tagId)
  deleteFromDb' conn (Delete "poststags" wh)
deleteDbTagForDrafts' conn tagId = do
  let wh = WherePair "tag_id=?" (Id tagId)
  deleteFromDb' conn (Delete "draftstags" wh)
isExistTag' conn tagId = do
  let wh = WherePair "tag_id=?" (Id tagId)
  isExistInDb' conn (Exists "tags" wh)
insertReturnTag' conn tagName = do
  let insPair = InsertPair "tag_name" (Txt tagName)
  insertReturn' conn (InsertRet "tags" [insPair] "tag_id")

workWithTags :: (MonadCatch m) => Handle m -> ReqInfo -> ExceptT ReqError m ResponseInfo
workWithTags h@Handle{..} (ReqInfo meth path qStr _) = 
  case (meth,path) of
    (POST,["tags"]) -> do
      lift $ logInfo hLog "Create tag command"
      tokenAdminAuth hAuth  req
      checkQStr hExist qStr >>= createTag h
    (GET,["tags",tagIdTxt]) -> do
      lift $ logInfo hLog "Get tag command"
      tagId <- checkTagResourse h tagIdTxt
      getTag h tagId
    (PUT,["tags",tagIdTxt]) -> do
      lift $ logInfo hLog  "Update tag command"
      tokenAdminAuth hAuth req
      tagId <- checkTagResourse h tagIdTxt
      checkQStr hExist qStr >>= updateTag h tagId
    (DELETE,["tags",tagIdTxt]) -> do
      lift $ logInfo hLog  "Delete tag command"
      tokenAdminAuth hAuth req
      tagId <- checkTagResourse h tagIdTxt
      deleteTag h tagId
    (x,y) -> throwE $ ResourseNotExistError $ "Unknown method-path combination: " ++ show (x,y)



createTag :: (Monad m, MonadCatch m) => Handle m -> CreateTag -> ExceptT ReqError m ResponseInfo
createTag Handle{..} (CreateTag tagNameParam) = do
  tagId <- catchInsRetE hLog $ insertReturnTag tagNameParam
  lift $ logInfo hLog $ "Tag_id: " ++ show tagId ++ " created"
  okHelper $ TagResponse tagId tagNameParam

getTag :: (Monad m, MonadCatch m) => Handle m -> TagId -> ExceptT ReqError m ResponseInfo
getTag Handle{..} tagId = do
  tagName <- catchOneSelE hLog logpair $ selectTagNames tagId
  lift $ logInfo hLog $ "Tag_id: " ++ show tagId ++ " sending in response"
  okHelper $ TagResponse tagId tagName

updateTag :: (Monad m, MonadCatch m) => Handle m -> TagId -> UpdateTag -> ExceptT ReqError m ResponseInfo
updateTag Handle{..} tagId (UpdateTag tagNameParam) = do
  catchUpdE hLog $ updateDbTag tagNameParam tagId
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagId ++ " updated"
  okHelper $ TagResponse tagId tagNameParam

deleteTag :: (Monad m, MonadCatch m) => Handle m -> TagId -> ExceptT ReqError m ResponseInfo
deleteTag Handle{..} tagId = do
  let deleteTgDr = deleteDbTagForDrafts tagId
  let deleteTgPos = deleteDbTagForPosts tagId
  let deleteTg = deleteDbTag tagId
  withTransactionDBE (deleteTgDr >> deleteTgPos >> deleteTg)
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagId ++ " deleted"
  okHelper $ OkResponse {ok = True}


checkTagResourse Handle{..} tagIdTxt =
  iD <- tryReadResourseId "tag_id" tagIdTxt
  isExistResourseE hExist (TagId iD)

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchTransactE (hLog h) . withTransactionDB h