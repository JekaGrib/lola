


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Tag where

import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT,throwE)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Logger
import Methods.Common
import Oops
import Api.Request.QueryStr (CreateTag (..), UpdateTag (..),checkQStr)
import Types
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE)
import Psql.ToQuery
import Network.HTTP.Types (StdMethod(..),QueryText)
import TryRead (tryReadResourseId)
import Api.Request.EndPoint
import Psql.ToQuery.Delete
import Psql.ToQuery.Exists
import Psql.ToQuery.Insert
import Psql.ToQuery.Select
import Psql.ToQuery.Update
import Psql.Methods.Common

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

