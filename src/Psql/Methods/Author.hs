{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Author where

import Api.Response (AuthorResponse (..), OkResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Database.PostgreSQL.Simple (withTransaction,Connection)
import Logger
import Methods.Common
import Methods.Common.DeleteMany (deleteAllAboutDrafts)
import qualified Methods.Common.DeleteMany (Handle, makeH)
import Psql.Selecty (Author (..))
import Oops
import Api.Request.QueryStr (CreateAuthor (..), UpdateAuthor (..),checkQStr)
import Types
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth,tokenUserAuth)
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

selectDraftsForAuthor' :: Connection -> AuthorId -> IO [DraftId]
selectDraftsForAuthor' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  selectOnly' conn (Select ["draft_id"] "drafts" wh)
selectAuthorsForUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  selectOnly' conn (Select ["author_id"] "authors" wh)
selectAuthors' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  select' conn (Select ["author_id", "author_info", "user_id"] "authors" wh)
updateDbAuthor' conn usId auInfo auId = do
  let set1 = SetPair "user_id=?" (Id usId)
  let set2 = SetPair "author_info=?" (Txt auInfo)
  let wh = WherePair "author_id=?" (Id auId)
  updateInDb' conn (Update "authors" [set1,set2] wh)
updateDbAuthorForPosts' conn auIdNew auId = do
  let set = SetPair "author_id=?" (Id auIdNew)
  let wh = WherePair "author_id=?" (Id auId)
  updateInDb' conn (Update "posts" [set] wh)
deleteDbAuthor' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  deleteFromDb' conn (Delete "authors" wh)
isUserAuthor' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  isExistInDb' conn (Exists "authors" wh)
insertReturnAuthor' conn usId auInfo = do
  let insPair1 = InsertPair "user_id"     (Id  usId)
  let insPair2 = InsertPair "author_info" (Txt  auInfo)
  let insPairs = [insPair1,insPair2]
  insertReturn' conn (InsertRet "authors" insPairs "author_id")

