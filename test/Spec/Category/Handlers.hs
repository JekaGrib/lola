{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Category.Handlers where

import Control.Monad.State (StateT (..), modify)
import Methods.Category
import qualified Spec.Auth.Handlers (handle)
import Spec.Category.Types
import Spec.Conf (defConf)
import qualified Spec.Exist.Handlers (handle)
import Spec.Log (handLogWarning)
import qualified Spec.MakeCatResp.Handlers (handle)
import Spec.Types (MockAction (..))
import Types

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

withTransactionDBTest :: StateT [MockAction] IO a -> StateT [MockAction] IO a
withTransactionDBTest m = do
  modify (TRANSACTIONOPEN :)
  a <- m
  modify (TRANSACTIONCLOSE :)
  return a

updateDbCatTest :: CatName -> CategoryId -> StateT [MockAction] IO ()
updateDbCatTest cN cId =
  modify (CatMock (UpdateDbCat cN cId) :)

updateDbSubCatTest :: CatName -> SuperCatId -> CategoryId -> StateT [MockAction] IO ()
updateDbSubCatTest cN supCId cId =
  modify (CatMock (UpdateDbSubCat cN supCId cId) :)

updateDbCatsForPostsTest :: CategoryId -> [CategoryId] -> StateT [MockAction] IO ()
updateDbCatsForPostsTest cId cIds =
  modify (CatMock (UpdateDbCatsForPosts cId cIds) :)

updateDbCatsForDraftsTest :: CategoryId -> [CategoryId] -> StateT [MockAction] IO ()
updateDbCatsForDraftsTest cId cIds =
  modify (CatMock (UpdateDbCatsForDrafts cId cIds) :)

deleteDbCatsTest :: [CategoryId] -> StateT [MockAction] IO ()
deleteDbCatsTest cIds =
  modify (CatMock (DeleteDbCats cIds) :)

insertReturnCatTest :: CatName -> StateT [MockAction] IO CategoryId
insertReturnCatTest cN = do
  modify (CatMock (InsertReturnCat cN) :)
  return 14

insertReturnSubCatTest :: CatName -> SuperCatId -> StateT [MockAction] IO CategoryId
insertReturnSubCatTest cN supCId = do
  modify (CatMock (InsertReturnSubCat cN supCId) :)
  return 14
