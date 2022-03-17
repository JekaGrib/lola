{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Tag.Handlers where

import Spec.Conf (defConf)
import Control.Monad.Catch (SomeException, catch, throwM)
import Spec.Log (handLogDebug)
import Methods.Tag
import Types
import Spec.Types (MockAction (..))
import qualified Spec.Auth.Handlers (handle)
import qualified Spec.Exist.Handlers (handle)
import Spec.Tag.Types
import Control.Monad.State (StateT (..), modify)

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
catchTransactionE m = 
  m `catch` (\(e :: SomeException) -> do
    modify (TRANSACTIONunROLL :)
    throwM e)

selectTagNamesTest :: TagId -> StateT [MockAction] IO [TagName]
selectTagNamesTest tagId = do
  modify (TagMock (SelectTagNames tagId) :)
  return ["cats"]

updateDbTagTest :: TagName -> TagId  -> StateT [MockAction] IO ()
updateDbTagTest tagName tagId = 
  modify (TagMock (UpdateDbTag tagName tagId) :)


deleteDbTagTest :: TagId  -> StateT [MockAction] IO ()
deleteDbTagTest tagId = 
  modify (TagMock (DeleteDbTag tagId) :)


deleteDbTagForDraftsTest :: TagId  -> StateT [MockAction] IO ()
deleteDbTagForDraftsTest tagId = 
  modify (TagMock (DeleteDbTagForDrafts tagId) :)
  
  

deleteDbTagForPostsTest :: TagId  -> StateT [MockAction] IO ()
deleteDbTagForPostsTest tagId = 
  modify (TagMock (DeleteDbTagForPosts tagId) :)
  

insertReturnTagTest :: TagName -> StateT [MockAction] IO TagId
insertReturnTagTest tagName = do
  modify (TagMock (InsertReturnTag tagName) :)
  return 14
  