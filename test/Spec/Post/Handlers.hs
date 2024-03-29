module Spec.Post.Handlers where

import Control.Monad.State (StateT (..), modify)
import Data.Time.Calendar (Day, fromGregorian)
import Methods.Post
import Psql.Selecty (Post (..), Tag (..))
import Psql.ToQuery.SelectLimit (Filter (..), OrderBy (..))
import qualified Spec.Auth.Handlers (handle)
import Spec.Conf (defConf)
import qualified Spec.DeleteMany.Handlers (handle)
import qualified Spec.Draft.Handlers (handle)
import qualified Spec.Exist.Handlers (handle)
import Spec.Log (handLogWarning)
import qualified Spec.MakeCatResp.Handlers (handle)
import Spec.Post.Types
import Spec.Types (MockAction (..))
import Types

handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogWarning
    selectPostsTest
    selectLimPostsTest
    selectPicsForPostTest
    selectTagsForPostTest
    selectUsersForPostTest
    withTransactionDBTest
    Spec.MakeCatResp.Handlers.handle
    Spec.DeleteMany.Handlers.handle
    Spec.Draft.Handlers.handle
    Spec.Auth.Handlers.handle
    Spec.Exist.Handlers.handle

withTransactionDBTest :: StateT [MockAction] IO a -> StateT [MockAction] IO a
withTransactionDBTest m = do
  modify (TRANSACTIONOPEN :)
  a <- m
  modify (TRANSACTIONCLOSE :)
  return a

selectPostsTest :: PostId -> StateT [MockAction] IO [Post]
selectPostsTest pId = do
  modify (PostMock (SelectPosts pId) :)
  return [Post pId 7 "author" 3 "post" dayExample 4 "lalala" 8]

dayExample :: Day
dayExample = fromGregorian 2020 02 02

selectLimPostsTest :: [Filter] -> OrderBy -> Page -> Limit -> StateT [MockAction] IO [Post]
selectLimPostsTest filt ordBy page lim = do
  modify (PostMock (SelectLimPosts filt ordBy page lim) :)
  return
    [ Post 1 7 "author" 3 "post" dayExample 4 "lalala" 8,
      Post 2 8 "author" 4 "post2" dayExample 2 "lalala" 7,
      Post 3 9 "author" 5 "post3" dayExample 1 "lalala" 6
    ]

selectPicsForPostTest :: PostId -> StateT [MockAction] IO [PictureId]
selectPicsForPostTest pId = do
  modify (PostMock (SelectPicsForPost pId) :)
  return [6, 9, 12]

selectTagsForPostTest :: PostId -> StateT [MockAction] IO [Tag]
selectTagsForPostTest pId = do
  modify (PostMock (SelectTagsForPost pId) :)
  return [Tag 15 "cats", Tag 18 "dogs", Tag 20 "birds"]

selectUsersForPostTest :: PostId -> StateT [MockAction] IO [UserId]
selectUsersForPostTest pId = do
  modify (PostMock (SelectUsersForPost pId) :)
  return [3]
