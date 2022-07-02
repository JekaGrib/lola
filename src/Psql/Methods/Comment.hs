module Psql.Methods.Comment where

import Database.PostgreSQL.Simple (Connection)
import Psql.Methods.Common
import Psql.Selecty (Comment (..))
import Psql.ToQuery.Delete (Delete (..))
import Psql.ToQuery.Insert (InsertPair (..), InsertRet (..))
import Psql.ToQuery.Select (Select (..), Where (..))
import Psql.ToQuery.SelectLimit (OrderBy, SelectLim (..))
import Psql.ToQuery.Update (Set (..), Update (..))
import Types

selectComment' :: Connection -> CommentId -> IO [Comment]
selectComment' conn commentId = do
  let wh = WherePair "comment_id=?" (Id commentId)
  select' conn (Select ["comment_id", "user_id", "comment_text", "post_id"] "comments" wh)

selectUsersForPost' :: Connection -> PostId -> IO [UserId]
selectUsersForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
      t = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
  selectOnly' conn (Select ["user_id"] t wh)

selectUsersForComment' :: Connection -> CommentId -> IO [UserId]
selectUsersForComment' conn commentId = do
  let wh = WherePair "comment_id=?" (Id commentId)
  selectOnly' conn (Select ["user_id"] "comments" wh)

selectPostsForComment' :: Connection -> CommentId -> IO [PostId]
selectPostsForComment' conn commentId = do
  let wh = WherePair "comment_id=?" (Id commentId)
  selectOnly' conn (Select ["post_id"] "comments" wh)

selectLimCommentsForPost' :: Connection -> PostId -> OrderBy -> Page -> Limit -> IO [Comment]
selectLimCommentsForPost' conn postId orderBy page limit = do
  let wh = WherePair "post_id=?" (Id postId)
  selectLimit' conn $
    SelectLim
      ["comment_id", "user_id", "comment_text", "post_id"]
      "comments"
      wh
      []
      orderBy
      page
      limit

updateDbComment' :: Connection -> CommentText -> CommentId -> IO ()
updateDbComment' conn commentTxt commentId = do
  let set = SetPair "comment_text=?" (Txt commentTxt)
  let wh = WherePair "comment_id=?" (Id commentId)
  updateInDb' conn (Update "comments" [set] wh)

deleteDbComment' :: Connection -> CommentId -> IO ()
deleteDbComment' conn commentId = do
  let wh = WherePair "comment_id=?" (Id commentId)
  deleteFromDb' conn (Delete "comments" wh)

insertReturnComment' :: Connection -> CommentText -> PostId -> UserId -> IO CommentId
insertReturnComment' conn commentTxt postId usId = do
  let insPairTxt = InsertPair "comment_text" (Txt commentTxt)
      insPairPost = InsertPair "post_id" (Id postId)
      insPairUser = InsertPair "user_id" (Id usId)
      insPairs = [insPairTxt, insPairPost, insPairUser]
  insertReturn' conn (InsertRet "comments" insPairs "comment_id")
