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
  let setUser = SetPair "user_id=?" (Id usId)
      setInfo = SetPair "author_info=?" (Txt auInfo)
      wh = WherePair "author_id=?" (Id auId)
  updateInDb' conn (Update "authors" [setUser, setInfo] wh)

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
  let insPairUser = InsertPair "user_id" (Id usId)
      insPairInfo = InsertPair "author_info" (Txt auInfo)
      insPairs = [insPairUser, insPairInfo]
  insertReturn' conn (InsertRet "authors" insPairs "author_id")
