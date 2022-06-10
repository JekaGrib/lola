module Psql.Methods.Author where

import Database.PostgreSQL.Simple (Connection)
import Psql.Methods.Common
import Psql.Selecty (Author (..))
import Psql.ToQuery.Delete (Delete (..))
import Psql.ToQuery.Exists (Exists (..))
import Psql.ToQuery.Insert (InsertPair (..), InsertRet (..))
import Psql.ToQuery.Select (Select (..), Where (..))
import Psql.ToQuery.Update (Set (..), Update (..))
import Types

selectDraftsForAuthor' :: Connection -> AuthorId -> IO [DraftId]
selectDraftsForAuthor' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  selectOnly' conn (Select ["draft_id"] "drafts" wh)

selectAuthorsForUser' :: Connection -> UserId -> IO [AuthorId]
selectAuthorsForUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  selectOnly' conn (Select ["author_id"] "authors" wh)

selectAuthors' :: Connection -> AuthorId -> IO [Author]
selectAuthors' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  select' conn (Select ["author_id", "author_info", "user_id"] "authors" wh)

updateDbAuthor' :: Connection -> UserId -> AuthorInfo -> AuthorId -> IO ()
updateDbAuthor' conn usId auInfo auId = do
  let set1 = SetPair "user_id=?" (Id usId)
  let set2 = SetPair "author_info=?" (Txt auInfo)
  let wh = WherePair "author_id=?" (Id auId)
  updateInDb' conn (Update "authors" [set1, set2] wh)

updateDbAuthorForPosts' :: Connection -> AuthorId -> AuthorId -> IO ()
updateDbAuthorForPosts' conn auIdNew auId = do
  let set = SetPair "author_id=?" (Id auIdNew)
  let wh = WherePair "author_id=?" (Id auId)
  updateInDb' conn (Update "posts" [set] wh)

deleteDbAuthor' :: Connection -> AuthorId -> IO ()
deleteDbAuthor' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  deleteFromDb' conn (Delete "authors" wh)

isUserAuthor' :: Connection -> UserId -> IO Bool
isUserAuthor' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  isExistInDb' conn (Exists "authors" wh)

insertReturnAuthor' :: Connection -> UserId -> AuthorInfo -> IO AuthorId
insertReturnAuthor' conn usId auInfo = do
  let insPair1 = InsertPair "user_id" (Id usId)
  let insPair2 = InsertPair "author_info" (Txt auInfo)
  let insPairs = [insPair1, insPair2]
  insertReturn' conn (InsertRet "authors" insPairs "author_id")
