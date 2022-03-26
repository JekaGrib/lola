{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.User where

import Database.PostgreSQL.Simple (Connection)
import Psql.Methods.Common
import Psql.Selecty (Auth (..), User (..))
import Psql.ToQuery.Delete (Delete (..))
import Psql.ToQuery.Insert (InsertPair (..), InsertRet (..))
import Psql.ToQuery.Select (Select (..), Where (..))
import Psql.ToQuery.Update (Set (..), Update (..))
import Types

selectUsers' :: Connection -> UserId -> IO [User]
selectUsers' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  select' conn $
    Select
      ["first_name", "last_name", "user_pic_id", "user_create_date"]
      "users"
      wh

selectAuthsForUser' :: Connection -> UserId -> IO [Auth]
selectAuthsForUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  select' conn $ Select ["password", "admin"] "users" wh

selectAuthorsForUser' :: Connection -> UserId -> IO [AuthorId]
selectAuthorsForUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  selectOnly' conn $ Select ["author_id"] "authors" wh

selectDraftsForAuthor' :: Connection -> AuthorId -> IO [DraftId]
selectDraftsForAuthor' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  selectOnly' conn $ Select ["draft_id"] "drafts" wh

updateDbUserForComms' :: Connection -> UserId -> UserId -> IO ()
updateDbUserForComms' conn newUsId usId = do
  let set = SetPair "user_id=?" (Id newUsId)
  let wh = WherePair "user_id=?" (Id usId)
  updateInDb' conn (Update "comments" [set] wh)

updateDbAuthorForPosts' :: Connection -> AuthorId -> AuthorId -> IO ()
updateDbAuthorForPosts' conn newAuId auId = do
  let set = SetPair "author_id=?" (Id newAuId)
  let wh = WherePair "author_id=?" (Id auId)
  updateInDb' conn (Update "posts" [set] wh)

updateDbTokenKeyForUser' :: Connection -> TokenKey -> UserId -> IO ()
updateDbTokenKeyForUser' conn tokenKey usId = do
  let set = SetPair "token_key=?" (Str tokenKey)
  let wh = WherePair "user_id=?" (Id usId)
  updateInDb' conn (Update "users" [set] wh)

deleteDbUser' :: Connection -> UserId -> IO ()
deleteDbUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  deleteFromDb' conn (Delete "users" wh)

deleteDbAuthor' :: Connection -> AuthorId -> IO ()
deleteDbAuthor' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  deleteFromDb' conn (Delete "authors" wh)

insertReturnUser' :: Connection -> InsertUser -> IO UserId
insertReturnUser' conn (InsertUser pwd fName lName picId day bool tokenKey) = do
  let insPair1 = InsertPair "password" (Txt pwd)
  let insPair2 = InsertPair "first_name" (Txt fName)
  let insPair3 = InsertPair "last_name" (Txt lName)
  let insPair4 = InsertPair "user_pic_id" (Id picId)
  let insPair5 = InsertPair "user_create_date" (Day day)
  let insPair6 = InsertPair "admin" (Bool bool)
  let insPair7 = InsertPair "token_key" (Str tokenKey)
  let insPairs = [insPair1, insPair2, insPair3, insPair4, insPair5, insPair6, insPair7]
  insertReturn' conn (InsertRet "users" insPairs "user_id")
