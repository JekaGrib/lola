{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Comment where

import Conf (Config (..), extractConn)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Logger
import Methods.Common.Auth (AccessMode (..))
import Methods.Common
import Psql.Selecty (Comment (comment_idC))
import Oops
import Api.Request.QueryStr (CreateComment (..), GetComments (..), UpdateComment (..),checkQStr)
import Types
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth,tokenUserAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE)
import Psql.ToQuery
import Network.HTTP.Types (StdMethod(..),QueryText)
import TryRead (tryReadResourseId)
import Api.Request.EndPoint
import Psql.ToQuery.Delete
import Psql.ToQuery.Exists
import Psql.ToQuery.Insert
import Psql.ToQuery.SelectLimit
import Psql.ToQuery.Select
import Psql.ToQuery.Update
import Psql.Methods.Common
import Database.PostgreSQL.Simple (Connection)

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
      ["comment_id","user_id","comment_text"]
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

