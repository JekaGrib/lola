module Spec.Category.Types where

import Types

data CatMock
  = UpdateDbCat CatName CategoryId
  | UpdateDbSubCat CatName SuperCatId CategoryId
  | UpdateDbCatsForPosts CategoryId [CategoryId]
  | UpdateDbCatsForDrafts CategoryId [CategoryId]
  | DeleteDbCats [CategoryId]
  | InsertReturnCat CatName
  | InsertReturnSubCat CatName SuperCatId
  deriving (Eq, Show)
