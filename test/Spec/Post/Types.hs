{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Post.Types where

import Psql.ToQuery.SelectLimit (Filter (..), OrderBy (..))
import Types

data PostMock
  = SelectPosts PostId
  | SelectLimPosts [Filter] OrderBy Page Limit
  | SelectPicsForPost PostId
  | SelectTagsForPost PostId
  | SelectUsersForPost PostId
  | SelectPostInfos PostId
  deriving (Eq, Show)
