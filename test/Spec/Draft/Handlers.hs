module Spec.Draft.Handlers where

import Control.Monad.State (StateT (..), modify)
import Data.Time.Calendar (Day, fromGregorian)
import Methods.Draft
import Psql.Selecty (Author (..), Draft (..), Tag (..))
import Psql.ToQuery.SelectLimit (OrderBy (..))
import qualified Spec.Auth.Handlers (handle)
import Spec.Conf (defConf)
import qualified Spec.DeleteMany.Handlers (handle)
import Spec.Draft.Types
import qualified Spec.Exist.Handlers (handle)
import Spec.Log (handLogWarning)
import qualified Spec.MakeCatResp.Handlers (handle)
import Spec.Types (MockAction (..))
import Types

handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogWarning
    selectDraftsTest
    selectUsersForDraftTest
    selectTagsTest
    selectLimDraftsForAuthorTest
    selectPicsForDraftTest
    selectTagsForDraftTest
    selectPostsForDraftTest
    selectAuthorsForUserTest
    updateDbDraftTest
    updateDbPostTest
    updateDbPostForDraftTest
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

getDayTest :: StateT [MockAction] IO Day
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
selectTagsTest tIds = do
  modify (DraftMock (SelectTags tIds) :)
  return $ zipWith Tag tIds tagsNames

tagsNames :: [TagName]
tagsNames = cycle ["cats", "dogs", "birds", "cows"]

dayExample :: Day
dayExample = fromGregorian 2020 02 02

selectLimDraftsForAuthorTest :: AuthorId -> OrderBy -> Page -> Limit -> StateT [MockAction] IO [Draft]
selectLimDraftsForAuthorTest aId ordBy page lim = do
  modify (DraftMock (SelectLimDraftsForAuthor aId ordBy page lim) :)
  let draft1 = Draft 1 "author" 7 "draft" 15 "lalala" 6
      draft5 = Draft 5 "author" 0 "draft5" 24 "lalala" 4
      draft12 = Draft 12 "author" 0 "draft12" 17 "lalala" 13
  return [draft1, draft5, draft12]

selectPicsForDraftTest :: PostId -> StateT [MockAction] IO [PictureId]
selectPicsForDraftTest pId = do
  modify (DraftMock (SelectPicsForDraft pId) :)
  return [6, 9, 12]

selectTagsForDraftTest :: DraftId -> StateT [MockAction] IO [Tag]
selectTagsForDraftTest dId = do
  modify (DraftMock (SelectTagsForDraft dId) :)
  return [Tag 15 "cats", Tag 18 "dogs", Tag 20 "birds"]

selectPostsForDraftTest :: DraftId -> StateT [MockAction] IO [PostId]
selectPostsForDraftTest dId = do
  modify (DraftMock (SelectPostsForDraft dId) :)
  return [7]

selectAuthorsForUserTest :: UserId -> StateT [MockAction] IO [Author]
selectAuthorsForUserTest 25 = do
  modify (DraftMock (SelectAuthorsForUser 25) :)
  return []
selectAuthorsForUserTest uId = do
  modify (DraftMock (SelectAuthorsForUser uId) :)
  return [Author 7 "author" uId]

updateDbDraftTest :: DraftId -> UpdateDbDraft -> StateT [MockAction] IO ()
updateDbDraftTest dId updDr =
  modify (DraftMock (UpdateDraft dId updDr) :)

updateDbPostTest :: PostId -> UpdateDbPost -> StateT [MockAction] IO ()
updateDbPostTest pId updPs =
  modify (DraftMock (UpdatePost pId updPs) :)

updateDbPostForDraftTest :: DraftId -> PostId -> StateT [MockAction] IO ()
updateDbPostForDraftTest draftId pId =
  modify (DraftMock (UpdatePostForDraft draftId pId) :)

insertReturnDraftTest :: InsertDraft -> StateT [MockAction] IO DraftId
insertReturnDraftTest insDr = do
  modify (DraftMock (InsertReturnDraft insDr) :)
  return 14

insertManyDraftsPicsTest :: [(DraftId, PictureId)] -> StateT [MockAction] IO ()
insertManyDraftsPicsTest xs =
  modify (DraftMock (InsertManyDraftsPics xs) :)

insertManyDraftsTagsTest :: [(DraftId, TagId)] -> StateT [MockAction] IO ()
insertManyDraftsTagsTest xs =
  modify (DraftMock (InsertManyDraftsTags xs) :)

insertReturnPostTest :: InsertPost -> StateT [MockAction] IO PostId
insertReturnPostTest insPs = do
  modify (DraftMock (InsertReturnPost insPs) :)
  return 20

insertManyPostsPicsTest :: [(PostId, PictureId)] -> StateT [MockAction] IO ()
insertManyPostsPicsTest xs =
  modify (DraftMock (InsertManyPostsPics xs) :)

insertManyPostsTagsTest :: [(PostId, TagId)] -> StateT [MockAction] IO ()
insertManyPostsTagsTest xs =
  modify (DraftMock (InsertManyPostsTags xs) :)
