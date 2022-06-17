module Psql.Methods.Draft where

import Database.PostgreSQL.Simple (Connection)
import Psql.Methods.Common
import Psql.Selecty (Author (..), Draft (..), Tag (..))
import Psql.ToQuery.Insert (InsertMany (..), InsertManyPair (..), InsertPair (..), InsertRet (..))
import Psql.ToQuery.Select (Select (..), Where (..))
import Psql.ToQuery.SelectLimit (OrderBy, SelectLim (..))
import Psql.ToQuery.Update (Set (..), Update (..))
import Types

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

selectLimDraftsForAuthor' :: Connection -> AuthorId -> OrderBy -> Page -> Limit -> IO [Draft]
selectLimDraftsForAuthor' conn auId orderBy page limit = do
  let wh = WherePair "drafts.author_id=?" (Id auId)
  selectLimit' conn $
    SelectLim
      ["drafts.draft_id", "author_info", "COALESCE (post_id, '0') AS post_id", "draft_name", "draft_category_id", "draft_text", "draft_main_pic_id"]
      "drafts JOIN authors ON authors.author_id = drafts.author_id"
      wh
      []
      orderBy
      page
      limit

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
  let setName = SetPair "draft_name=?" (Txt name)
      setCat = SetPair "draft_category_id=?" (Id catId)
      setTxt = SetPair "draft_text=?" (Txt txt)
      setPic = SetPair "draft_main_pic_id=?" (Id picId)
      wh = WherePair "draft_id=?" (Id drId)
  updateInDb' conn (Update "drafts" [setName, setCat, setTxt, setPic] wh)

updateDbPost' :: Connection -> PostId -> UpdateDbPost -> IO ()
updateDbPost' conn postId (UpdateDbPost name catId txt picId) = do
  let setName = SetPair "post_name=?" (Txt name)
      setCat = SetPair "post_category_id=?" (Id catId)
      setTxt = SetPair "post_text=?" (Txt txt)
      setPic = SetPair "post_main_pic_id=?" (Id picId)
      wh = WherePair "post_id=?" (Id postId)
  updateInDb' conn (Update "posts" [setName, setCat, setTxt, setPic] wh)

updateDbPostForDraft' :: Connection -> DraftId -> PostId -> IO ()
updateDbPostForDraft' conn draftId postId = do
  let set = SetPair "post_id=?" (Id postId)
      wh = WherePair "draft_id=?" (Id draftId)
  updateInDb' conn (Update "drafts" [set] wh)

insertReturnDraft' :: Connection -> InsertDraft -> IO DraftId
insertReturnDraft' conn insDraft = do
  let insPairs = insDraftToInsPairs insDraft
  insertReturn' conn (InsertRet "drafts" insPairs "draft_id")

insDraftToInsPairs :: InsertDraft -> [InsertPair]
insDraftToInsPairs (InsertDraft maybePostId auId drName catId drTxt picId) =
  let insPairAuthor = InsertPair "author_id" (Id auId)
      insPairName = InsertPair "draft_name" (Txt drName)
      insPairCat = InsertPair "draft_category_id" (Id catId)
      insPairTxt = InsertPair "draft_text" (Txt drTxt)
      insPairPic = InsertPair "draft_main_pic_id" (Id picId)
      insPairsList = [insPairAuthor, insPairName, insPairCat, insPairTxt, insPairPic]
   in case maybePostId of
        Nothing -> insPairsList
        Just postId -> insPairPostId : insPairsList
          where
            insPairPostId = InsertPair "post_id" (Id postId)

insertManyDraftsPics' :: Connection -> [(DraftId, PictureId)] -> IO ()
insertManyDraftsPics' conn xs = do
  let insPair = InsertManyPair ("draft_id", "pic_id") xs
  insertMany' conn (InsertMany "draftspics" insPair)

insertManyDraftsTags' :: Connection -> [(DraftId, TagId)] -> IO ()
insertManyDraftsTags' conn xs = do
  let insPair = InsertManyPair ("draft_id", "tag_id") xs
  insertMany' conn (InsertMany "draftstags" insPair)

insertReturnPost' :: Connection -> InsertPost -> IO PostId
insertReturnPost' conn (InsertPost auId name day catId txt picId) = do
  let insPairAuthor = InsertPair "author_id" (Id auId)
      insPairName = InsertPair "post_name" (Txt name)
      insPairDay = InsertPair "post_create_date" (Day day)
      insPairCat = InsertPair "post_category_id" (Id catId)
      insPairTxt = InsertPair "post_text" (Txt txt)
      insPairPic = InsertPair "post_main_pic_id" (Id picId)
      insPairs = [insPairAuthor, insPairName, insPairDay, insPairCat, insPairTxt, insPairPic]
  insertReturn' conn (InsertRet "posts" insPairs "post_id")

insertManyPostsPics' :: Connection -> [(PostId, PictureId)] -> IO ()
insertManyPostsPics' conn xs = do
  let insPair = InsertManyPair ("post_id", "pic_id") xs
  insertMany' conn (InsertMany "postspics" insPair)

insertManyPostsTags' :: Connection -> [(PostId, TagId)] -> IO ()
insertManyPostsTags' conn xs = do
  let insPair = InsertManyPair ("post_id", "tag_id") xs
  insertMany' conn (InsertMany "poststags" insPair)
