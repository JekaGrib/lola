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

selectComm' :: Connection -> CommentId -> IO [Comment]
selectComm' conn commId = do
  let wh = WherePair "comment_id=?" (Id commId)
  select' conn (Select ["comment_id", "user_id", "comment_text", "post_id"] "comments" wh)

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

selectLimCommsForPost' :: Connection -> PostId -> OrderBy -> Page -> Limit -> IO [Comment]
selectLimCommsForPost' conn postId orderBy page limit = do
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
  let insPairTxt = InsertPair "comment_text" (Txt commTxt)
      insPairPost = InsertPair "post_id" (Id postId)
      insPairUser = InsertPair "user_id" (Id usId)
      insPairs = [insPairTxt, insPairPost, insPairUser]
  insertReturn' conn (InsertRet "comments" insPairs "comment_id")
