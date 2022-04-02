{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Comment.Types where

import Psql.ToQuery.SelectLimit (OrderBy (..))
import Types

data CommMock
  = SelectComm CommentId
  | SelectUsersForPost PostId
  | SelectUsersForComm CommentId
  | SelectPostsForComm CommentId
  | SelectLimCommsForPost PostId OrderBy Page Limit
  | UpdateDbComm CommentText CommentId
  | DeleteDbComm CommentId
  | InsertReturnComm CommentText PostId UserId
  deriving (Eq, Show)
