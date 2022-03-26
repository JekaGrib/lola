{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Tag where

import Database.PostgreSQL.Simple (Connection)
import Psql.Methods.Common
import Psql.ToQuery.Delete (Delete (..))
import Psql.ToQuery.Insert (InsertPair (..), InsertRet (..))
import Psql.ToQuery.Select (Select (..), Where (..))
import Psql.ToQuery.Update (Set (..), Update (..))
import Types

selectTagNames' :: Connection -> TagId -> IO [TagName]
selectTagNames' conn tagId = do
  let wh = WherePair "tag_id=?" (Id tagId)
  selectOnly' conn (Select ["tag_name"] "tags" wh)

updateDbTag' :: Connection -> TagName -> TagId -> IO ()
updateDbTag' conn tagName tagId = do
  let set = SetPair "tag_name=?" (Txt tagName)
  let wh = WherePair "tag_id=?" (Id tagId)
  updateInDb' conn (Update "tags" [set] wh)

deleteDbTag' :: Connection -> TagId -> IO ()
deleteDbTag' conn tagId = do
  let wh = WherePair "tag_id=?" (Id tagId)
  deleteFromDb' conn (Delete "tags" wh)

deleteDbTagForPosts' :: Connection -> TagId -> IO ()
deleteDbTagForPosts' conn tagId = do
  let wh = WherePair "tag_id=?" (Id tagId)
  deleteFromDb' conn (Delete "poststags" wh)

deleteDbTagForDrafts' :: Connection -> TagId -> IO ()
deleteDbTagForDrafts' conn tagId = do
  let wh = WherePair "tag_id=?" (Id tagId)
  deleteFromDb' conn (Delete "draftstags" wh)

insertReturnTag' :: Connection -> TagName -> IO TagId
insertReturnTag' conn tagName = do
  let insPair = InsertPair "tag_name" (Txt tagName)
  insertReturn' conn (InsertRet "tags" [insPair] "tag_id")
