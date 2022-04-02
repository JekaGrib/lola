{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Tag.Handlers where

import Control.Monad.Catch (throwM)
import Control.Monad.State (StateT (..), modify)
import Database.PostgreSQL.Simple (ExecStatus (FatalError), SqlError (..))
import Methods.Tag
import Oops (UnexpectedDbOutPutException (..))
import qualified Spec.Auth.Handlers (handle0)
import Spec.Conf (defConf)
import qualified Spec.Exist.Handlers (handle)
import Spec.Log (handLogDebug)
import Spec.Tag.Types
import Spec.Types (MockAction (..))
import Types

handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogDebug
    (selectTagNamesTest ["cats"])
    updateDbTagTest
    deleteDbTagTest
    deleteDbTagForDraftsTest
    deleteDbTagForPostsTest
    insertReturnTagTest
    withTransactionDBTest
    Spec.Auth.Handlers.handle0
    Spec.Exist.Handlers.handle

throwSqlEx :: StateT [MockAction] IO a
throwSqlEx = throwM $ SqlError "oops" FatalError "oops" "oops" "oops"

handle1 :: Handle (StateT [MockAction] IO)
handle1 = handle {selectTagNames = selectTagNamesTest ["cats", "food"]}

handle2 :: Handle (StateT [MockAction] IO)
handle2 = handle {selectTagNames = selectTagNamesTest []}

handle3 :: Handle (StateT [MockAction] IO)
handle3 =
  handle
    { selectTagNames = selectTagNamesTestEx,
      insertReturnTag = insertReturnTagTestEx,
      updateDbTag = updateDbTagTestEx,
      deleteDbTag = deleteDbTagTestEx
    }

handle4 :: Handle (StateT [MockAction] IO)
handle4 =
  handle
    { deleteDbTagForDrafts = deleteDbTagForDraftsTestEx
    }

handle5 :: Handle (StateT [MockAction] IO)
handle5 =
  handle
    { insertReturnTag = insertReturnTagTestEx1
    }

withTransactionDBTest :: StateT [MockAction] IO a -> StateT [MockAction] IO a
withTransactionDBTest m = do
  modify (TRANSACTIONOPEN :)
  a <- m
  modify (TRANSACTIONCLOSE :)
  return a

selectTagNamesTest :: [TagName] -> TagId -> StateT [MockAction] IO [TagName]
selectTagNamesTest xs tagId = do
  modify (TagMock (SelectTagNames tagId) :)
  return xs

updateDbTagTest :: TagName -> TagId -> StateT [MockAction] IO ()
updateDbTagTest tagName tagId =
  modify (TagMock (UpdateDbTag tagName tagId) :)

deleteDbTagTest :: TagId -> StateT [MockAction] IO ()
deleteDbTagTest tagId =
  modify (TagMock (DeleteDbTag tagId) :)

deleteDbTagForDraftsTest :: TagId -> StateT [MockAction] IO ()
deleteDbTagForDraftsTest tagId =
  modify (TagMock (DeleteDbTagForDrafts tagId) :)

deleteDbTagForPostsTest :: TagId -> StateT [MockAction] IO ()
deleteDbTagForPostsTest tagId =
  modify (TagMock (DeleteDbTagForPosts tagId) :)

insertReturnTagTest :: TagName -> StateT [MockAction] IO TagId
insertReturnTagTest tagName = do
  modify (TagMock (InsertReturnTag tagName) :)
  return 14

selectTagNamesTestEx :: TagId -> StateT [MockAction] IO [TagName]
selectTagNamesTestEx _ = throwSqlEx

deleteDbTagTestEx :: TagId -> StateT [MockAction] IO ()
deleteDbTagTestEx _ = throwSqlEx

deleteDbTagForDraftsTestEx :: TagId -> StateT [MockAction] IO ()
deleteDbTagForDraftsTestEx _ = throwSqlEx

updateDbTagTestEx :: TagName -> TagId -> StateT [MockAction] IO ()
updateDbTagTestEx _ _ = throwSqlEx

insertReturnTagTestEx :: TagName -> StateT [MockAction] IO TagId
insertReturnTagTestEx _ = throwSqlEx

insertReturnTagTestEx1 :: TagName -> StateT [MockAction] IO TagId
insertReturnTagTestEx1 _ = throwM UnexpectedEmptyDbOutPutException
