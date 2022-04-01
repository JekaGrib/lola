{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Post.Types where

import Types
import Psql.ToQuery.SelectLimit (OrderBy (..),Filter(..))

data PostMock
  = SelectPosts PostId 
  | SelectLimPosts [Filter] OrderBy Page Limit 
  | SelectPicsForPost PostId 
  | SelectTagsForPost PostId 
  | SelectUsersForPost PostId 
  | SelectPostInfos PostId 
  deriving (Eq, Show)
