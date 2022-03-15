{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Category where

import Api.Response (CatResponse (..), OkResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.List (intercalate)
import Database.PostgreSQL.Simple (Connection)
import Logger
import Methods.Common
import Methods.Common.MakeCatResp (findOneLevelSubCats, makeCatResp)
import qualified Methods.Common.MakeCatResp (Handle, makeH)
import Oops
import Api.Request.QueryStr (CreateCategory (..), UpdateCategory (..),checkQStr)
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


updateDbCat' :: Connection -> CatName -> CategoryId -> IO ()
updateDbCat' conn catName catId = do
  let set = SetPair "category_name=?" (Txt catName)
  let wh = WherePair "category_id=?" (Id catId)
  updateInDb' conn (Update "categories" [set] wh)

updateDbSubCat' :: Connection -> CatName -> SuperCatId -> CategoryId -> IO ()
updateDbSubCat' conn catName superCatId catId = do
  let set1 = SetPair "category_name=?" (Txt catName)
  let set2 = SetPair "super_category_id=?" (Id superCatId)
  let wh = WherePair "category_id=?" (Id catId)
  updateInDb' conn (Update "categories" [set1,set2] wh)

updateDbCatsForPosts' :: Connection -> CategoryId -> [CategoryId] -> IO ()
updateDbCatsForPosts' conn newCatId catIds = do
  let set = SetPair "post_category_id=?" (Id newCatId)
  let toWhPair catId = WherePair "post_category_id=?" (Id catId)
  let wh = WhereOr $ map toWhPair catIds
  updateInDb' conn (Update "posts" [set] wh)

updateDbCatsForDrafts' :: Connection -> CategoryId -> [CategoryId] -> IO ()
updateDbCatsForDrafts' conn newCatId catIds = do
  let set = SetPair "draft_category_id=?" (Id newCatId)
  let toWhPair catId = WherePair "draft_category_id=?" (Id catId)
  let wh = WhereOr $ map toWhPair catIds
  updateInDb' conn (Update "drafts" [set] wh)

deleteDbCats' :: Connection -> [CategoryId] -> IO ()
deleteDbCats' conn catIds = do
  let toWhPair catId = WherePair "category_id=?" (Id catId)
  let wh = WhereOr $ map toWhPair catIds
  deleteFromDb' conn (Delete "categories" wh)

insertReturnCat' :: Connection -> CatName -> IO CategoryId
insertReturnCat' conn catName = do
  let insPair = InsertPair "category_name" (Txt catName)
  insertReturn' conn (InsertRet "categories" [insPair] "category_id")

insertReturnSubCat' :: Connection -> CatName -> SuperCatId -> IO CategoryId
insertReturnSubCat' conn catName superCatId = do
  let insPair1 = InsertPair "category_name" (Txt catName)
  let insPair2 = InsertPair "super_category_id" (Id superCatId)
  insertReturn' conn (InsertRet "categories" [insPair1,insPair2] "category_id")

