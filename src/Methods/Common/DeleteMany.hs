{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Methods.Common.DeleteMany where

import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Data.List (intercalate)
import Methods.Common
import Types
import Methods.Common.ToQuery


data Handle m = Handle
  { hConf :: Config
  , selectDraftsForPost :: PostId -> m [DraftId]
  , deleteDbPicsForPost :: PostId -> m ()
  , deleteDbTagsForPost :: PostId -> m ()
  , deleteDbCommsForPost :: PostId -> m ()
  , deleteDbPost :: PostId -> m ()
  , deleteDbPicsForDrafts :: [DraftId] -> m ()
  , deleteDbTagsForDrafts :: [DraftId] -> m ()
  , deleteDbDrafts :: [DraftId] -> m ()
  }

makeH :: Config -> Handle IO
makeH conf =
  let conn = extractConn conf
   in Handle
        conf
        (selectDraftsForPost' conn)
        (deleteDbPicsForPost' conn)
        (deleteDbTagsForPost' conn)
        (deleteDbCommsForPost' conn)
        (deleteDbPost' conn)
        (deleteDbPicsForDrafts' conn)
        (deleteDbTagsForDrafts' conn)
        (deleteDbDrafts' conn)

selectDraftsForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  selectOnly' conn (Select ["draft_id"] "drafts" wh)
deleteDbPicsForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  deleteFromDb' conn (Delete "postspics" wh)
deleteDbTagsForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  deleteFromDb' conn (Delete "poststags" wh)
deleteDbCommsForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  deleteFromDb' conn (Delete "comments" wh)
deleteDbPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  deleteFromDb' conn (Delete "posts" wh)
deleteDbPicsForDrafts' conn draftIds = do
  let toWhPair draftId = WherePair "draft_id=?" (Id draftId)
  let wh = WhereOr $ map toWhPair draftIds
  deleteFromDb' conn (Delete "draftspics" wh)
deleteDbTagsForDrafts' conn draftIds = do
  let toWhPair draftId = WherePair "draft_id=?" (Id draftId)
  let wh = WhereOr $ map toWhPair draftIds
  deleteFromDb' conn (Delete "draftstags" wh)
deleteDbDrafts' conn draftIds = do
  let toWhPair draftId = WherePair "draft_id=?" (Id draftId)
  let wh = WhereOr $ map toWhPair draftIds
  deleteFromDb' conn (Delete "drafts" wh)


deleteAllAboutPost :: (MonadCatch m) => Handle m -> PostId -> m ()
deleteAllAboutPost h@Handle{..} postId = do
  deletePicsTagsForPost h postId
  deleteDbCommsForPost postId
  draftsIds <- selectDraftsForPost postId
  deleteAllAboutDrafts h draftsIds
  deleteDbPost postId

deletePicsTagsForPost :: (MonadCatch m) => Handle m -> PostId -> m ()
deletePicsTagsForPost Handle{..} postId = do
  deleteDbPicsForPost postId
  deleteDbTagsForPost postId

deleteAllAboutDrafts :: (MonadCatch m) => Handle m -> [DraftId] -> m ()
deleteAllAboutDrafts _ [] = return ()
deleteAllAboutDrafts h@Handle{..} draftsIds = do
  deletePicsTagsForDrafts h draftsIds
  deleteDbDrafts draftsIds

deletePicsTagsForDrafts :: (MonadCatch m) => Handle m -> [DraftId] -> m ()
deletePicsTagsForDrafts _ [] = return ()
deletePicsTagsForDrafts Handle{..} draftsIds = do
  deleteDbPicsForDrafts draftsIds
  deleteDbTagsForDrafts draftsIds

