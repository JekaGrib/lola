{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Category.Handlers where

import Control.Monad.Catch (throwM)
import Control.Monad.State (StateT (..), modify)
import Database.PostgreSQL.Simple (ExecStatus (FatalError), SqlError (..))
import Methods.Category
import qualified Spec.Auth.Handlers (handle)
import Spec.Conf (defConf)
import qualified Spec.Exist.Handlers (handle)
import qualified Spec.MakeCatResp.Handlers (handle)
import Spec.Log (handLogWarning)
import Spec.Category.Types 
import Spec.Types (MockAction (..))
import Types
import Oops (UnexpectedDbOutPutException(..))


handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogWarning
    updateDbCatTest
    updateDbSubCatTest
    updateDbCatsForPostsTest
    updateDbCatsForDraftsTest
    deleteDbCatsTest
    insertReturnCatTest
    insertReturnSubCatTest
    withTransactionDBTest
    Spec.MakeCatResp.Handlers.handle
    Spec.Auth.Handlers.handle
    Spec.Exist.Handlers.handle

throwSqlEx :: StateT [MockAction] IO a
throwSqlEx = throwM $ SqlError "oops" FatalError "oops" "oops" "oops"


withTransactionDBTest :: StateT [MockAction] IO a -> StateT [MockAction] IO a
withTransactionDBTest m = do
  modify (TRANSACTIONOPEN :)
  a <- m
  modify (TRANSACTIONCLOSE :)
  return a

updateDbCatTest :: CatName -> CategoryId -> StateT [MockAction] IO ()
updateDbCatTest cN cId = do
  modify ( CatMock (UpdateDbCat cN cId) :)

updateDbSubCatTest :: CatName -> SuperCatId -> CategoryId -> StateT [MockAction] IO ()
updateDbSubCatTest cN supCId cId  = do
  modify ( CatMock (UpdateDbSubCat cN supCId cId) :)

updateDbCatsForPostsTest :: CategoryId -> [CategoryId] -> StateT [MockAction] IO ()
updateDbCatsForPostsTest cId cIds  = do
  modify ( CatMock (UpdateDbCatsForPosts cId cIds) :)

updateDbCatsForDraftsTest :: CategoryId -> [CategoryId] -> StateT [MockAction] IO ()
updateDbCatsForDraftsTest cId cIds  = do
  modify ( CatMock (UpdateDbCatsForDrafts cId cIds) :)

deleteDbCatsTest :: [CategoryId] -> StateT [MockAction] IO ()
deleteDbCatsTest cIds  = do
  modify ( CatMock (DeleteDbCats cIds) :)

insertReturnCatTest :: CatName -> StateT [MockAction] IO CategoryId
insertReturnCatTest cN  = do
  modify ( CatMock (InsertReturnCat cN) :)
  return 14

insertReturnSubCatTest :: CatName -> SuperCatId -> StateT [MockAction] IO CategoryId
insertReturnSubCatTest cN supCId  = do
  modify ( CatMock (InsertReturnSubCat cN supCId) :)
  return 14
  