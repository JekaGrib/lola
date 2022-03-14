{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Common.DeleteMany where

import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Data.List (intercalate)
import Methods.Common
import Types
import Psql.ToQuery
import Psql.Methods.Common
import Psql.ToQuery.Delete
import Psql.ToQuery.Exists
import Psql.ToQuery.Insert
import Psql.ToQuery.SelectLimit
import Psql.ToQuery.Select
import Psql.ToQuery.Update


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


