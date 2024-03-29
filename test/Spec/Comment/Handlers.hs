module Spec.Comment.Handlers where

import Control.Monad.State (StateT (..), modify)
import Methods.Comment
import Psql.Selecty (Comment (..))
import Psql.ToQuery.SelectLimit (OrderBy (..))
import qualified Spec.Auth.Handlers (handle)
import Spec.Comment.Types
import Spec.Conf (defConf)
import qualified Spec.Exist.Handlers (handle)
import Spec.Log (handLogWarning)
import Spec.Types (MockAction (..))
import Types

handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogWarning
    selectCommTest
    selectUsersForPostTest
    selectUsersForCommTest
    selectPostsForCommTest
    selectLimCommsForPostTest
    updateDbCommTest
    deleteDbCommTest
    insertReturnCommTest
    Spec.Auth.Handlers.handle
    Spec.Exist.Handlers.handle

selectCommTest :: CommentId -> StateT [MockAction] IO [Comment]
selectCommTest cId = do
  modify (CommMock (SelectComm cId) :)
  return [Comment cId 3 "cool" 7]

selectUsersForPostTest :: PostId -> StateT [MockAction] IO [UserId]
selectUsersForPostTest pId = do
  modify (CommMock (SelectUsersForPost pId) :)
  return [2]

selectUsersForCommTest :: CommentId -> StateT [MockAction] IO [UserId]
selectUsersForCommTest cId = do
  modify (CommMock (SelectUsersForComm cId) :)
  return [3]

selectPostsForCommTest :: CommentId -> StateT [MockAction] IO [PostId]
selectPostsForCommTest cId = do
  modify (CommMock (SelectPostsForComm cId) :)
  return [7]

selectLimCommsForPostTest :: PostId -> OrderBy -> Page -> Limit -> StateT [MockAction] IO [Comment]
selectLimCommsForPostTest pId ordBy page lim = do
  modify (CommMock (SelectLimCommsForPost pId ordBy page lim) :)
  return [Comment 1 3 "cool" pId, Comment 2 4 "ok" pId, Comment 3 5 "yes" pId]

updateDbCommTest :: CommentText -> CommentId -> StateT [MockAction] IO ()
updateDbCommTest cTxt cId =
  modify (CommMock (UpdateDbComm cTxt cId) :)

deleteDbCommTest :: CommentId -> StateT [MockAction] IO ()
deleteDbCommTest cId =
  modify (CommMock (DeleteDbComm cId) :)

insertReturnCommTest :: CommentText -> PostId -> UserId -> StateT [MockAction] IO CommentId
insertReturnCommTest cTxt pId uId = do
  modify (CommMock (InsertReturnComm cTxt pId uId) :)
  return 14
