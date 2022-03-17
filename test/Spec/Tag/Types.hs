{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Tag.Types where


import Types

data TagMock = 
  SelectTagNames TagId
  | UpdateDbTag TagName TagId
  | DeleteDbTag TagId
  | DeleteDbTagForDrafts TagId
  | DeleteDbTagForPosts TagId
  | InsertReturnTag TagName

  deriving (Eq, Show)



