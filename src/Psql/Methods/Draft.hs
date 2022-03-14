{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Draft where

import Api.Request.JSON (DraftRequest (..),checkDraftReqJson)
import Api.Response (AuthorResponse (..), CatResponse (..), DraftResponse (..), DraftsResponse (..), OkResponse (..), PicIdUrl (pic_idPU), PostIdOrNull (..), PostResponse (..), TagResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.List (intercalate, zip4)
import Database.PostgreSQL.Simple (withTransaction)
import Logger
import Methods.Category (fromCatResp)
import Methods.Common
import Methods.Common.DeleteMany (deleteAllAboutDrafts, deletePicsTagsForDrafts, deletePicsTagsForPost)
import qualified Methods.Common.DeleteMany (Handle, makeH)
import Methods.Common.MakeCatResp (makeCatResp)
import qualified Methods.Common.MakeCatResp (Handle, makeH)
import Psql.Selecty (Author (..), Draft (..), PostInfo (..), Tag (..))
import Oops
import Api.Request.QueryStr ( GetDrafts (..),checkQStr)
import Types
import Data.Time.Calendar ( Day)
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth,tokenUserAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE)
import Psql.ToQuery
import Network.HTTP.Types (StdMethod(..),QueryText)
import TryRead (tryReadResourseId)
import Api.Request.EndPoint
import Data.ByteString (ByteString)
import Psql.ToQuery.Delete
import Psql.ToQuery.Exists
import Psql.ToQuery.Insert
import Psql.ToQuery.Select
import Psql.ToQuery.SelectLimit
import Psql.ToQuery.Update
import Psql.Methods.Common




selectDrafts' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  select' conn $
    Select 
      ["d.draft_id", "author_info", "COALESCE (post_id, '0') AS post_id", "draft_name", "draft_category_id", "draft_text", "draft_main_pic_id"]
      "drafts AS d JOIN authors AS a ON d.author_id=a.author_id" 
      wh
selectUsersForDraft' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  selectOnly' conn $
    Select 
      ["user_id"]
      "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
      wh
selectTags' conn tagIds = do
  let toWhPair tagId = WherePair "tag_id=?" (Id tagId)
  let wh = WhereOr $ map toWhPair tagIds
  select' conn $
    Select 
      ["tag_id", "tag_name"]
      "tags"
      wh
selectDaysForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  selectOnly' conn (Select ["post_create_date"] "posts" wh)
selectLimDraftsForAuthor' conn auId orderBy page limit = do
  let wh = WherePair "author_id=?" (Id auId)
  selectLimit' conn $ 
    SelectLim 
      ["drafts.draft_id", "author_info", "COALESCE (post_id, '0') AS post_id", "draft_name", "draft_category_id", "draft_text", "draft_main_pic_id"]
      "drafts JOIN authors ON authors.author_id = drafts.author_id" 
      wh [] orderBy  page limit
selectPicsForDraft' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  selectOnly' conn (Select ["pic_id"] "draftspics" wh)
selectTagsForDraft' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  select' conn $ 
    Select 
      ["tags.tag_id", "tag_name"] 
      "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" 
      wh
selectPostsForDraft' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  selectOnly' conn $ 
    Select 
      ["COALESCE (post_id, '0') AS post_id"] 
      "drafts" 
      wh
selectAuthorsForUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  select' conn $ 
    Select 
    ["author_id", "author_info", "user_id"] 
    "authors" 
    wh
updateDbDraft' conn drId (UpdateDbDraft name catId txt picId) = do
  let set1 = SetPair "draft_name=?"        (Txt name)
  let set2 = SetPair "draft_category_id=?" (Id catId)
  let set3 = SetPair "draft_text=?"        (Txt txt)
  let set4 = SetPair "draft_main_pic_id=?" (Id picId)
  let wh = WherePair "draft_id=?"          (Id drId)
  updateInDb' conn (Update "drafts" [set1,set2,set3,set4] wh)
updateDbPost' conn postId (UpdateDbPost name catId txt picId) = do
  let set1 = SetPair "post_name=?"        (Txt name)
  let set2 = SetPair "post_category_id=?" (Id catId)
  let set3 = SetPair "post_text=?"        (Txt txt)
  let set4 = SetPair "post_main_pic_id=?" (Id picId)
  let wh = WherePair "post_id=?"          (Id postId)
  updateInDb' conn (Update "posts" [set1,set2,set3,set4] wh)

insertReturnDraft' conn (InsertDraft Nothing auId drName catId drTxt picId) = do
  let insPair1 = InsertPair "author_id"         (Id  auId)
  let insPair2 = InsertPair "draft_name"        (Txt drName)
  let insPair3 = InsertPair "draft_category_id" (Id  catId)
  let insPair4 = InsertPair "draft_text"        (Txt drTxt)
  let insPair5 = InsertPair "draft_main_pic_id" (Id  picId)
  let insPairs = [insPair1,insPair2,insPair3,insPair4,insPair5]
  insertReturn' conn (InsertRet "drafts" insPairs "draft_id")
insertReturnDraft' conn (InsertDraft (Just postId) auId drName catId drTxt picId) = do
  let insPair1 = InsertPair "author_id"         (Id  auId)
  let insPair2 = InsertPair "draft_name"        (Txt drName)
  let insPair3 = InsertPair "draft_category_id" (Id  catId)
  let insPair4 = InsertPair "draft_text"        (Txt drTxt)
  let insPair5 = InsertPair "draft_main_pic_id" (Id  picId)
  let insPair6 = InsertPair "post_id"           (Id  postId)
  let insPairs = [insPair1,insPair2,insPair3,insPair4,insPair5,insPair6]
  insertReturn' conn (InsertRet "drafts" insPairs "draft_id")
insertManyDraftsPics' conn xs = do
  let insPair = InsertManyPair  ("draft_id", "pic_id") xs
  insertMany' conn (InsertMany "draftspics" insPair)
insertManyDraftsTags' conn xs = do
  let insPair = InsertManyPair  ("draft_id", "tag_id") xs
  insertMany' conn (InsertMany "draftstags" insPair)
insertReturnPost' conn (InsertPost auId name day catId txt picId) = do
  let insPair1 = InsertPair "author_id"        (Id  auId)
  let insPair2 = InsertPair "post_name"        (Txt name)
  let insPair3 = InsertPair "post_create_date" (Day day)
  let insPair4 = InsertPair "post_category_id" (Id  catId)
  let insPair5 = InsertPair "post_text"        (Txt txt)
  let insPair6 = InsertPair "post_main_pic_id" (Id  picId)
  let insPairs = [insPair1,insPair2,insPair3,insPair4,insPair5,insPair6]
  insertReturn' conn (InsertRet "posts" insPairs "post_id")
insertManyPostsPics' conn xs = do
  let insPair = InsertManyPair  ("post_id", "pic_id") xs
  insertMany' conn (InsertMany "postspics" insPair)
insertManyPostsTags' conn xs = do
  let insPair = InsertManyPair  ("post_id", "tag_id") xs
  insertMany' conn (InsertMany "poststags" insPair)


