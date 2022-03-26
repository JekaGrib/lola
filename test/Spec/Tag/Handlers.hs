{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Tag.Handlers where

import Spec.Conf (defConf)
import Control.Monad.Catch (SomeException, catchAll, catch, throwM,bracket_,mask_,mask,onError,onException,bracketOnError)
import Spec.Log (handLogDebug)
import Methods.Tag
import Types
import Spec.Types (MockAction (..))
import qualified Spec.Auth.Handlers (handle)
import qualified Spec.Exist.Handlers (handle)
import Spec.Tag.Types
import Control.Monad.State (StateT (..), modify)
import Database.PostgreSQL.Simple (SqlError(..), ExecStatus(FatalError))

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
    Spec.Auth.Handlers.handle
    Spec.Exist.Handlers.handle

throwSqlEx :: StateT [MockAction] IO a
throwSqlEx = throwM $ SqlError "oops" FatalError "oops" "oops" "oops"

handle1 :: Handle (StateT [MockAction] IO)
handle1 = handle {selectTagNames = selectTagNamesTest ["cats","food"]}

handle2 :: Handle (StateT [MockAction] IO)
handle2 = handle {selectTagNames = selectTagNamesTest []}

handle3 :: Handle (StateT [MockAction] IO)
handle3 = 
  handle 
  { insertReturnTag = insertReturnTagTestEx
  , updateDbTag = updateDbTagTestEx
  , deleteDbTag = deleteDbTagTestEx
  }

handle4 :: Handle (StateT [MockAction] IO)
handle4 = 
  handle 
  {  deleteDbTagForDrafts = deleteDbTagForDraftsTestEx
  }

withTransactionDBTest :: StateT [MockAction] IO a -> StateT [MockAction] IO a
withTransactionDBTest m = do
  modify (TRANSACTIONOPEN :)
  a <- catchTransactionE m
  modify (TRANSACTIONCLOSE :)
  return a

withTransactionDBTest9 :: StateT [MockAction] IO a -> StateT [MockAction] IO a
withTransactionDBTest9 m = maskMy m

maskMy :: StateT [MockAction] IO a ->  StateT [MockAction] IO a
maskMy m = mask $ \restore -> do
  modify (TRANSACTIONOPEN :)
  a <- restore m  `onException`  (modify (TRANSACTIONunROLL :))
  modify (TRANSACTIONCLOSE :)
  return a


withTransactionDBTest1 :: StateT [MockAction] IO a -> StateT [MockAction] IO a
withTransactionDBTest1 m = do
  a <- bracketOnError 
    (modify (TRANSACTIONOPEN :))
    (\_ -> (modify (TRANSACTIONunROLL :)))
    (\_ -> m ) 
  modify (TRANSACTIONCLOSE :)
  return a

catchTransactionE :: StateT [MockAction] IO a -> StateT [MockAction] IO a
catchTransactionE m = 
  m `catchAll` (\e -> do
    modify (TRANSACTIONunROLL :)
    throwM e)

selectTagNamesTest :: [TagName] -> TagId -> StateT [MockAction] IO [TagName]
selectTagNamesTest xs tagId = do
  modify (TagMock (SelectTagNames tagId) :)
  return xs

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

deleteDbTagTestEx :: TagId -> StateT [MockAction] IO ()
deleteDbTagTestEx _ = throwSqlEx

deleteDbTagForDraftsTestEx :: TagId -> StateT [MockAction] IO ()
deleteDbTagForDraftsTestEx _ = throwSqlEx

updateDbTagTestEx :: TagName -> TagId -> StateT [MockAction] IO ()
updateDbTagTestEx _ _ = throwSqlEx

insertReturnTagTestEx :: TagName -> StateT [MockAction] IO TagId
insertReturnTagTestEx _ = throwSqlEx

