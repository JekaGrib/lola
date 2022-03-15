{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Tag.Handlers where

import Spec.Conf (defConf)
import Control.Monad.Catch (SomeException, catch, throwM)
import Control.Monad.State (StateT (..), withStateT,modify)
import Data.Text (Text, unpack)
import Spec.Log (handLogDebug)
import Methods.Tag
import Spec.Oops (UnexpectedArgsException (..))
import Spec.TestDB
import Types
import Spec.Types (MockAction (..))
import qualified Spec.Auth.Handlers (handle)
import qualified Spec.Exist.Handlers (handle)
import Spec.Tag.Types

handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogDebug
    selectTagNamesTest
    updateDbTagTest
    deleteDbTagTest
    deleteDbTagForDraftsTest
    deleteDbTagForPostsTest
    insertReturnTagTest
    withTransactionDBTest
    Spec.Auth.Handlers.handle
    Spec.Exist.Handlers.handle


withTransactionDBTest :: StateT [MockAction] IO a -> StateT [MockAction] IO a
withTransactionDBTest m = do
  modify (TRANSACTIONOPEN :)
  a <- catchTransactionE m 
  modify (TRANSACTIONCLOSE :)
  return a

catchTransactionE :: StateT [MockAction] IO a -> StateT [MockAction] IO a
catchTransactionE m = do
  m `catch` (\(e :: SomeException) -> do
    modify (TRANSACTIONunROLL :)
    throwM e)

selectTagNamesTest :: TagId -> StateT [MockAction] IO [TagName]
selectTagNamesTest tagId = StateT $ \acts ->
  return (["cats"],TagMock (SelectTagNames tagId):acts)

updateDbTagTest :: TagName -> TagId  -> StateT [MockAction] IO ()
updateDbTagTest tagName tagId = StateT $  \acts ->
   return ((), TagMock (UpdateDbTag tagName tagId) : acts)

deleteDbTagTest :: TagId  -> StateT [MockAction] IO ()
deleteDbTagTest tagId = StateT $ \acts ->
  return $ ((), TagMock (DeleteDbTag tagId) : acts)

deleteDbTagForDraftsTest :: TagId  -> StateT [MockAction] IO ()
deleteDbTagForDraftsTest tagId = StateT $ \acts ->
  return $ ((), TagMock (DeleteDbTagForDrafts tagId) : acts)

deleteDbTagForPostsTest :: TagId  -> StateT [MockAction] IO ()
deleteDbTagForPostsTest tagId = StateT $ \acts ->
  return $ ((), TagMock (DeleteDbTagForPosts tagId) : acts)

insertReturnTagTest :: TagName -> StateT [MockAction] IO TagId
insertReturnTagTest tagName = StateT $ \acts ->
  return $ (14,TagMock (InsertReturnTag tagName) : acts)