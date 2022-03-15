{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Methods.Common.Exist.UncheckedExId  where

import Methods.Common
import Psql.ToQuery
import Control.Monad.Trans.Except (ExceptT,throwE)
import Oops (ReqError(..))
import Control.Monad.Catch (MonadCatch)
import Types
import Conf (Config (..), extractConn)
import Control.Monad (unless)


data UncheckedExId =
  AuthorId AuthorId
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




