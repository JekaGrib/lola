module Methods.Common.Exist.UncheckedExId where

import Types

data UncheckedExId
  = AuthorId AuthorId
  | CategoryId CategoryId
  | CommentId CommentId
  | DraftId DraftId
  | PictureId PictureId
  | PostId PostId
  | TagId TagId
  | UserId UserId
  deriving (Eq, Show)

class ToPretty a where
  toPretty :: a -> String

instance ToPretty UncheckedExId where
  toPretty (AuthorId iD) = "author_id: " ++ show iD
  toPretty (CategoryId iD) = "category_id: " ++ show iD
  toPretty (CommentId iD) = "comment_id: " ++ show iD
  toPretty (DraftId iD) = "draft_id: " ++ show iD
  toPretty (PictureId iD) = "pic_id: " ++ show iD
  toPretty (PostId iD) = "post_id: " ++ show iD
  toPretty (TagId iD) = "tag_id: " ++ show iD
  toPretty (UserId iD) = "user_id: " ++ show iD
