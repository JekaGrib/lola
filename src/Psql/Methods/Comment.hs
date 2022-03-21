{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Comment where

import Psql.Selecty (Comment (..))
import Types
import Psql.ToQuery.Delete (Delete(..))
import Psql.ToQuery.Insert (InsertRet(..),InsertPair(..))
import Psql.ToQuery.SelectLimit (SelectLim(..),OrderBy)
import Psql.ToQuery.Select (Select(..),Where(..))
import Psql.ToQuery.Update (Update(..),Set(..))
import Psql.Methods.Common
import Database.PostgreSQL.Simple (Connection)


selectComm' :: Connection -> CommentId -> IO [Comment]
selectComm' conn commId = do
  let wh = WherePair "comment_id=?" (Id commId)
  select' conn (Select ["comment_id","user_id","comment_text","post_id"] "comments" wh)

selectUsersForPost' :: Connection -> PostId -> IO [UserId]
selectUsersForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  selectOnly' conn (Select ["user_id"] "posts AS p JOIN authors AS a ON p.author_id=a.author_id" wh)

selectUsersForComm' :: Connection -> CommentId -> IO [UserId]
selectUsersForComm' conn commId = do
  let wh = WherePair "comment_id=?" (Id commId)
  selectOnly' conn (Select ["user_id"] "comments" wh)

selectPostsForComm' :: Connection -> CommentId -> IO [PostId]
selectPostsForComm' conn commId = do
  let wh = WherePair "comment_id=?" (Id commId)
  selectOnly' conn (Select ["post_id"] "comments" wh)

selectLimCommsForPost' :: Connection -> PostId -> OrderBy -> Page -> Limit ->  IO [Comment]
selectLimCommsForPost' conn postId orderBy page limit = do
  let wh = WherePair "post_id=?" (Id postId)
  selectLimit' conn $ 
    SelectLim 
      ["comment_id","user_id","comment_text","post_id"]
      "comments" wh [] orderBy page limit

updateDbComm' :: Connection -> CommentText -> CommentId -> IO ()
updateDbComm' conn commTxt commId = do
  let set = SetPair "comment_text=?" (Txt commTxt)
  let wh = WherePair "comment_id=?" (Id commId)
  updateInDb' conn (Update "comments" [set] wh)

deleteDbComm' :: Connection -> CommentId -> IO ()
deleteDbComm' conn commId = do
  let wh = WherePair "comment_id=?" (Id commId)
  deleteFromDb' conn (Delete "comments" wh)

insertReturnComm' :: Connection -> CommentText -> PostId -> UserId -> IO CommentId
insertReturnComm' conn commTxt postId usId = do
  let insPair1 = InsertPair "comment_text" (Txt commTxt)
  let insPair2 = InsertPair "post_id" (Id postId)
  let insPair3 = InsertPair "user_id" (Id usId)
  let insPairs = [insPair1,insPair2,insPair3]
  insertReturn' conn (InsertRet "comments" insPairs "comment_id")

