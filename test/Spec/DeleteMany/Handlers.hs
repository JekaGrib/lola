{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.DeleteMany.Handlers where

import Control.Monad.State (StateT (..), modify)
import Methods.Common.DeleteMany (Handle (..))
import Spec.Conf (defConf)
import Spec.DeleteMany.Types
import Spec.Types (MockAction (..))
import Types

handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    selectDraftsForPostTest
    deleteDbPicsForPostTest
    deleteDbTagsForPostTest
    deleteDbCommsForPostTest
    deleteDbPostTest
    deleteDbPicsForDraftsTest
    deleteDbTagsForDraftsTest
    deleteDbDraftsTest

selectDraftsForPostTest :: PostId -> StateT [MockAction] IO [DraftId]
selectDraftsForPostTest pId = do
  modify (DeleteManyMock (SelectDraftsForPost pId) :)
  return [5, 7]

deleteDbPicsForPostTest :: PostId -> StateT [MockAction] IO ()
deleteDbPicsForPostTest pId =
  modify (DeleteManyMock (DeleteDbPicsForPost pId) :)

deleteDbTagsForPostTest :: PostId -> StateT [MockAction] IO ()
deleteDbTagsForPostTest pId =
  modify (DeleteManyMock (DeleteDbTagsForPost pId) :)

deleteDbCommsForPostTest :: PostId -> StateT [MockAction] IO ()
deleteDbCommsForPostTest pId =
  modify (DeleteManyMock (DeleteDbCommsForPost pId) :)

deleteDbPostTest :: PostId -> StateT [MockAction] IO ()
deleteDbPostTest pId =
  modify (DeleteManyMock (DeleteDbPost pId) :)

deleteDbPicsForDraftsTest :: [DraftId] -> StateT [MockAction] IO ()
deleteDbPicsForDraftsTest dIds =
  modify (DeleteManyMock (DeleteDbPicsForDrafts dIds) :)

deleteDbTagsForDraftsTest :: [DraftId] -> StateT [MockAction] IO ()
deleteDbTagsForDraftsTest dIds =
  modify (DeleteManyMock (DeleteDbTagsForDrafts dIds) :)

deleteDbDraftsTest :: [DraftId] -> StateT [MockAction] IO ()
deleteDbDraftsTest dIds =
  modify (DeleteManyMock (DeleteDbDrafts dIds) :)
