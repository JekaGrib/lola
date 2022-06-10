module Spec.DeleteMany.Types where

import Types

data DeleteManyMock
  = SelectDraftsForPost PostId
  | DeleteDbPicsForPost PostId
  | DeleteDbTagsForPost PostId
  | DeleteDbCommsForPost PostId
  | DeleteDbPost PostId
  | DeleteDbPicsForDrafts [DraftId]
  | DeleteDbTagsForDrafts [DraftId]
  | DeleteDbDrafts [DraftId]
  deriving (Eq, Show)
