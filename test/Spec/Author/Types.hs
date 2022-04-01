{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Author.Types where

import Types

data AuthorMock
  = SelectDraftsForAuthor AuthorId 
  | SelectAuthorsForUser UserId 
  | SelectAuthors AuthorId 
  | UpdateDbAuthor UserId AuthorInfo AuthorId 
  | UpdateDbAuthorForPosts AuthorId AuthorId 
  | DeleteDbAuthor AuthorId 
  | IsUserAuthor UserId 
  | InsertReturnAuthor UserId AuthorInfo 
  deriving (Eq, Show)
