module Spec.Draft.Types where

import Psql.ToQuery.SelectLimit (OrderBy (..))
import Types

data DraftMock
  = SelectDrafts DraftId
  | SelectUsersForDraft DraftId
  | SelectTags [TagId]
  | SelectLimDraftsForAuthor AuthorId OrderBy Page Limit
  | SelectPicsForDraft PostId
  | SelectTagsForDraft DraftId
  | SelectPostsForDraft DraftId
  | SelectAuthorsForUser UserId
  | UpdateDraft DraftId UpdateDbDraft
  | UpdatePost PostId UpdateDbPost
  | UpdatePostForDraft DraftId PostId 
  | InsertReturnDraft InsertDraft
  | InsertManyDraftsPics [(DraftId, PictureId)]
  | InsertManyDraftsTags [(DraftId, TagId)]
  | InsertReturnPost InsertPost
  | InsertManyPostsPics [(PostId, PictureId)]
  | InsertManyPostsTags [(PostId, TagId)]
  deriving (Eq, Show)
