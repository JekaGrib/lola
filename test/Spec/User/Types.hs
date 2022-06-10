module Spec.User.Types where

import Types

data UserMock
  = SelectUsers UserId
  | SelectAuthsForUser UserId
  | SelectAuthorsForUser UserId
  | SelectDraftsForAuthor AuthorId
  | UpdateDbUserForComms UserId UserId
  | UpdateDbAuthorForPosts AuthorId AuthorId
  | UpdateDbTokenKeyForUser TokenKey UserId
  | DeleteDbUser UserId
  | DeleteDbAuthor AuthorId
  | InsertReturnUser InsertUser
  | GetDay
  | GenerateTokenKey
  deriving (Eq, Show)
