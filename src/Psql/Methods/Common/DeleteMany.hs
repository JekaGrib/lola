{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Common.DeleteMany where

import Database.PostgreSQL.Simple (Connection)
import Psql.Methods.Common
import Psql.ToQuery.Delete (Delete (..))
import Psql.ToQuery.Select (Select (..), Where (..))
import Types

selectDraftsForPost' :: Connection -> PostId -> IO [DraftId]
selectDraftsForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  selectOnly' conn (Select ["draft_id"] "drafts" wh)

deleteDbPicsForPost' :: Connection -> PostId -> IO ()
deleteDbPicsForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  deleteFromDb' conn (Delete "postspics" wh)

deleteDbTagsForPost' :: Connection -> PostId -> IO ()
deleteDbTagsForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  deleteFromDb' conn (Delete "poststags" wh)

deleteDbCommsForPost' :: Connection -> PostId -> IO ()
deleteDbCommsForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  deleteFromDb' conn (Delete "comments" wh)

deleteDbPost' :: Connection -> PostId -> IO ()
deleteDbPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  deleteFromDb' conn (Delete "posts" wh)

deleteDbPicsForDrafts' :: Connection -> [DraftId] -> IO ()
deleteDbPicsForDrafts' conn draftIds = do
  let toWhPair draftId = WherePair "draft_id=?" (Id draftId)
  let wh = WhereOr $ map toWhPair draftIds
  deleteFromDb' conn (Delete "draftspics" wh)

deleteDbTagsForDrafts' :: Connection -> [DraftId] -> IO ()
deleteDbTagsForDrafts' conn draftIds = do
  let toWhPair draftId = WherePair "draft_id=?" (Id draftId)
  let wh = WhereOr $ map toWhPair draftIds
  deleteFromDb' conn (Delete "draftstags" wh)

deleteDbDrafts' :: Connection -> [DraftId] -> IO ()
deleteDbDrafts' conn draftIds = do
  let toWhPair draftId = WherePair "draft_id=?" (Id draftId)
  let wh = WhereOr $ map toWhPair draftIds
  deleteFromDb' conn (Delete "drafts" wh)
