{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Draft where

import Database.PostgreSQL.Simple (Connection)
import Psql.Selecty (Author (..), Draft (..), Tag (..))
import Types
import Data.Time.Calendar (Day)
import Psql.ToQuery.Insert (InsertRet(..),InsertPair(..),InsertMany(..),InsertManyPair(..))
import Psql.ToQuery.Select (Select(..),Where(..))
import Psql.ToQuery.SelectLimit (SelectLim(..),OrderBy)
import Psql.ToQuery.Update (Update(..),Set(..))
import Psql.Methods.Common



selectDrafts' :: Connection -> DraftId -> IO [Draft]
selectDrafts' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  select' conn $
    Select 
      ["d.draft_id", "author_info", "COALESCE (post_id, '0') AS post_id", "draft_name", "draft_category_id", "draft_text", "draft_main_pic_id"]
      "drafts AS d JOIN authors AS a ON d.author_id=a.author_id" 
      wh

selectUsersForDraft' :: Connection -> DraftId -> IO [UserId]
selectUsersForDraft' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  selectOnly' conn $
    Select 
      ["user_id"]
      "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
      wh

selectTags' :: Connection -> [TagId] -> IO [Tag]
selectTags' conn tagIds = do
  let toWhPair tagId = WherePair "tag_id=?" (Id tagId)
  let wh = WhereOr $ map toWhPair tagIds
  select' conn $
    Select 
      ["tag_id", "tag_name"]
      "tags"
      wh

selectDaysForPost' :: Connection -> PostId -> IO [Day]
selectDaysForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  selectOnly' conn (Select ["post_create_date"] "posts" wh)

selectLimDraftsForAuthor' :: Connection -> AuthorId -> OrderBy -> Page -> Limit ->  IO [Draft]
selectLimDraftsForAuthor' conn auId orderBy page limit = do
  let wh = WherePair "author_id=?" (Id auId)
  selectLimit' conn $ 
    SelectLim 
      ["drafts.draft_id", "author_info", "COALESCE (post_id, '0') AS post_id", "draft_name", "draft_category_id", "draft_text", "draft_main_pic_id"]
      "drafts JOIN authors ON authors.author_id = drafts.author_id" 
      wh [] orderBy  page limit

selectPicsForDraft' :: Connection -> DraftId -> IO [PictureId]
selectPicsForDraft' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  selectOnly' conn (Select ["pic_id"] "draftspics" wh)

selectTagsForDraft' :: Connection -> DraftId -> IO [Tag]
selectTagsForDraft' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  select' conn $ 
    Select 
      ["tags.tag_id", "tag_name"] 
      "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" 
      wh

selectPostsForDraft' :: Connection -> DraftId -> IO [PostId]
selectPostsForDraft' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  selectOnly' conn $ 
    Select 
      ["COALESCE (post_id, '0') AS post_id"] 
      "drafts" 
      wh

selectAuthorsForUser' :: Connection -> UserId -> IO [Author]
selectAuthorsForUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  select' conn $ 
    Select 
    ["author_id", "author_info", "user_id"] 
    "authors" 
    wh

updateDbDraft' :: Connection -> DraftId -> UpdateDbDraft -> IO ()
updateDbDraft' conn drId (UpdateDbDraft name catId txt picId) = do
  let set1 = SetPair "draft_name=?"        (Txt name)
  let set2 = SetPair "draft_category_id=?" (Id catId)
  let set3 = SetPair "draft_text=?"        (Txt txt)
  let set4 = SetPair "draft_main_pic_id=?" (Id picId)
  let wh = WherePair "draft_id=?"          (Id drId)
  updateInDb' conn (Update "drafts" [set1,set2,set3,set4] wh)

updateDbPost' :: Connection -> PostId -> UpdateDbPost -> IO ()
updateDbPost' conn postId (UpdateDbPost name catId txt picId) = do
  let set1 = SetPair "post_name=?"        (Txt name)
  let set2 = SetPair "post_category_id=?" (Id catId)
  let set3 = SetPair "post_text=?"        (Txt txt)
  let set4 = SetPair "post_main_pic_id=?" (Id picId)
  let wh = WherePair "post_id=?"          (Id postId)
  updateInDb' conn (Update "posts" [set1,set2,set3,set4] wh)

insertReturnDraft' ::  Connection -> InsertDraft -> IO DraftId
insertReturnDraft' conn insDraft = do
  let insPairs = insDraftToInsPairs insDraft
  insertReturn' conn (InsertRet "drafts" insPairs "draft_id")

insDraftToInsPairs :: InsertDraft -> [InsertPair]
insDraftToInsPairs (InsertDraft maybePostId auId drName catId drTxt picId) =
  let insPair1 = InsertPair "author_id"         (Id  auId)
      insPair2 = InsertPair "draft_name"        (Txt drName)
      insPair3 = InsertPair "draft_category_id" (Id  catId)
      insPair4 = InsertPair "draft_text"        (Txt drTxt)
      insPair5 = InsertPair "draft_main_pic_id" (Id  picId)
  in case maybePostId of
    Nothing     -> [insPair1,insPair2,insPair3,insPair4,insPair5]
    Just postId -> [insPair1,insPair2,insPair3,insPair4,insPair5,insPair6]
      where 
        insPair6 = InsertPair "post_id"         (Id  postId)


insertManyDraftsPics' :: Connection -> [(DraftId,PictureId)] -> IO ()
insertManyDraftsPics' conn xs = do
  let insPair = InsertManyPair  ("draft_id", "pic_id") xs
  insertMany' conn (InsertMany "draftspics" insPair)

insertManyDraftsTags' :: Connection -> [(DraftId,TagId)] -> IO ()
insertManyDraftsTags' conn xs = do
  let insPair = InsertManyPair  ("draft_id", "tag_id") xs
  insertMany' conn (InsertMany "draftstags" insPair)

insertReturnPost' ::  Connection -> InsertPost -> IO PostId
insertReturnPost' conn (InsertPost auId name day catId txt picId) = do
  let insPair1 = InsertPair "author_id"        (Id  auId)
  let insPair2 = InsertPair "post_name"        (Txt name)
  let insPair3 = InsertPair "post_create_date" (Day day)
  let insPair4 = InsertPair "post_category_id" (Id  catId)
  let insPair5 = InsertPair "post_text"        (Txt txt)
  let insPair6 = InsertPair "post_main_pic_id" (Id  picId)
  let insPairs = [insPair1,insPair2,insPair3,insPair4,insPair5,insPair6]
  insertReturn' conn (InsertRet "posts" insPairs "post_id")

insertManyPostsPics' :: Connection -> [(PostId,PictureId)] -> IO ()
insertManyPostsPics' conn xs = do
  let insPair = InsertManyPair  ("post_id", "pic_id") xs
  insertMany' conn (InsertMany "postspics" insPair)

insertManyPostsTags' :: Connection -> [(PostId,TagId)] -> IO ()
insertManyPostsTags' conn xs = do
  let insPair = InsertManyPair  ("post_id", "tag_id") xs
  insertMany' conn (InsertMany "poststags" insPair)


