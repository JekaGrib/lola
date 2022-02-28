


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


createTag :: (Monad m, MonadCatch m) => Handle m -> CreateTag -> ExceptT ReqError m ResponseInfo
createTag Handle{..} (CreateTag tagNameParam) = do
  tagId <- catchInsRetE hLog $ insertReturnTag tagNameParam
  lift $ logInfo hLog $ "Tag_id: " ++ show tagId ++ " created"
  okHelper $ TagResponse tagId tagNameParam

getTag :: (Monad m, MonadCatch m) => Handle m -> TagId -> ExceptT ReqError m ResponseInfo
getTag Handle{..} tagIdNum = do
  let logpair = ("tag_id", tagIdNum)
  tagName <- catchOneSelIfExistE hLog logpair $ selectTagNames tagIdNum
  lift $ logInfo hLog $ "Tag_id: " ++ show tagIdNum ++ " sending in response"
  okHelper $ TagResponse tagIdNum tagName

updateTag :: (Monad m, MonadCatch m) => Handle m -> UpdateTag -> ExceptT ReqError m ResponseInfo
updateTag Handle{..} (UpdateTag tagIdNum tagNameParam) = do
  let logpair = ("tag_id", tagIdNum)
  catchExistE hLog logpair $ isExistTag tagIdNum
  catchUpdE hLog $ updateDbTag tagNameParam tagIdNum
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagIdNum ++ " updated"
  okHelper $ TagResponse tagIdNum tagNameParam

deleteTag :: (Monad m, MonadCatch m) => Handle m -> DeleteTag -> ExceptT ReqError m ResponseInfo
deleteTag Handle{..} (DeleteTag tagIdNum) = do
  let logpair = ("tag_id", tagIdNum)
  catchExistE hLog logpair $ isExistTag tagIdNum
  let deleteTgDr = deleteDbTagForDrafts tagIdNum
  let deleteTgPos = deleteDbTagForPosts tagIdNum
  let deleteTg = deleteDbTag tagIdNum
  withTransactionDBE (deleteTgDr >> deleteTgPos >> deleteTg)
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagIdNum ++ " deleted"
  okHelper $ OkResponse {ok = True}

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchTransactE (hLog h) . withTransactionDB h