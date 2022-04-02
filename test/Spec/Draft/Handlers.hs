{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Draft.Handlers where

import Control.Monad.State (StateT (..), modify)
import Methods.Draft
import qualified Spec.Auth.Handlers (handle)
import Spec.Conf (defConf)
import qualified Spec.Exist.Handlers (handle)
import qualified Spec.MakeCatResp.Handlers (handle)
import qualified Spec.DeleteMany.Handlers (handle)
import Spec.Log (handLogWarning)
import Spec.Draft.Types
import Spec.Types (MockAction (..))
import Types
import Psql.ToQuery.SelectLimit (OrderBy (..))
import Psql.Selecty (Draft(..),Tag(..),Author(..),)
import Data.Time.Calendar (Day,fromGregorian)


handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogWarning
    selectDraftsTest
    selectUsersForDraftTest
    selectTagsTest
    selectDaysForPostTest
    selectLimDraftsForAuthorTest
    selectPicsForDraftTest
    selectTagsForDraftTest
    selectPostsForDraftTest
    selectAuthorsForUserTest
    updateDbDraftTest
    updateDbPostTest
    insertReturnDraftTest
    insertManyDraftsPicsTest
    insertManyDraftsTagsTest
    insertReturnPostTest
    insertManyPostsPicsTest
    insertManyPostsTagsTest
    getDayTest
    withTransactionDBTest
    Spec.MakeCatResp.Handlers.handle
    Spec.DeleteMany.Handlers.handle
    Spec.Auth.Handlers.handle
    Spec.Exist.Handlers.handle

withTransactionDBTest :: StateT [MockAction] IO a -> StateT [MockAction] IO a
withTransactionDBTest m = do
  modify (TRANSACTIONOPEN :)
  a <- m
  modify (TRANSACTIONCLOSE :)
  return a

getDayTest ::  StateT [MockAction] IO Day
getDayTest = do
  modify (GETDay :)
  return dayExample

selectDraftsTest :: DraftId -> StateT [MockAction] IO [Draft]
selectDraftsTest 25 = do
  modify (DraftMock (SelectDrafts 25) :)
  return [Draft 25 "author" 0 "draft" 9 "lalala" 6]
selectDraftsTest dId = do
  modify (DraftMock (SelectDrafts dId) :)
  return [Draft dId "author" 7 "draft" 9 "lalala" 6]


selectUsersForDraftTest :: DraftId -> StateT [MockAction] IO [UserId]
selectUsersForDraftTest dId = do
  modify (DraftMock (SelectUsersForDraft dId) :)
  return [3]


selectTagsTest :: [TagId] -> StateT [MockAction] IO [Tag]
selectTagsTest tIds  = do
  modify (DraftMock (SelectTags tIds) :)
  return $ zipWith toTag tIds tagsNames

tagsNames :: [TagName]
tagsNames = cycle ["cats","dogs","birds","cows"]

toTag :: TagId -> TagName -> Tag
toTag iD name = Tag iD name

selectDaysForPostTest :: PostId -> StateT [MockAction] IO [Day]
selectDaysForPostTest pId = do
  modify (DraftMock (SelectDaysForPost pId) :)
  return [dayExample]

dayExample :: Day
dayExample = (fromGregorian 2020 02 02)


selectLimDraftsForAuthorTest :: AuthorId -> OrderBy -> Page -> Limit -> StateT [MockAction] IO [Draft]
selectLimDraftsForAuthorTest aId ordBy page lim  = do
  modify (DraftMock (SelectLimDraftsForAuthor aId ordBy page lim) :)
  return [Draft 1 "author" 7 "draft" 15 "lalala" 6,Draft 5 "author" 0 "draft5" 24 "lalala" 4,Draft 12 "author" 0 "draft12" 17 "lalala" 13]

selectPicsForDraftTest :: PostId -> StateT [MockAction] IO [PictureId]
selectPicsForDraftTest pId = do
  modify (DraftMock (SelectPicsForDraft pId) :)
  return [6,9,12]


selectTagsForDraftTest :: DraftId -> StateT [MockAction] IO [Tag]
selectTagsForDraftTest dId = do
  modify (DraftMock (SelectTagsForDraft dId) :)
  return [Tag 15 "cats",Tag 18 "dogs",Tag 20 "birds"]

selectPostsForDraftTest :: DraftId -> StateT [MockAction] IO [PostId]
selectPostsForDraftTest dId = do
  modify (DraftMock (SelectPostsForDraft dId) :)
  return [7]

selectAuthorsForUserTest :: UserId -> StateT [MockAction] IO [Author]
selectAuthorsForUserTest uId = do
  modify (DraftMock (SelectAuthorsForUser uId) :)
  return [Author 7 "author" uId]

updateDbDraftTest :: DraftId -> UpdateDbDraft -> StateT [MockAction] IO ()
updateDbDraftTest dId updDr = do
  modify (DraftMock (UpdateDraft dId updDr) :)
  

updateDbPostTest :: PostId -> UpdateDbPost -> StateT [MockAction] IO ()
updateDbPostTest pId updPs = do
  modify (DraftMock (UpdatePost pId updPs) :)


insertReturnDraftTest :: InsertDraft -> StateT [MockAction] IO DraftId
insertReturnDraftTest insDr = do
  modify (DraftMock (InsertReturnDraft insDr) :)
  return 14

insertManyDraftsPicsTest :: [(DraftId, PictureId)] -> StateT [MockAction] IO ()
insertManyDraftsPicsTest xs = do
  modify (DraftMock (InsertManyDraftsPics xs) :)


insertManyDraftsTagsTest :: [(DraftId, TagId)] -> StateT [MockAction] IO ()
insertManyDraftsTagsTest xs = do
  modify (DraftMock (InsertManyDraftsTags xs) :)


insertReturnPostTest :: InsertPost -> StateT [MockAction] IO PostId
insertReturnPostTest insPs = do
  modify (DraftMock (InsertReturnPost insPs) :)
  return 20

insertManyPostsPicsTest :: [(PostId, PictureId)] -> StateT [MockAction] IO ()
insertManyPostsPicsTest xs  = do
  modify (DraftMock (InsertManyPostsPics xs) :)


insertManyPostsTagsTest :: [(PostId, TagId)] -> StateT [MockAction] IO ()
insertManyPostsTagsTest xs  = do
  modify (DraftMock (InsertManyPostsTags xs) :)
