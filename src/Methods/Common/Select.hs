{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Common.Select where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Types

class (Show a, FromRow a) => Select a

data Auth = Auth {pwdAu :: Text, admBoolAu :: Bool}
  deriving (Eq, Show)

instance FromRow Auth where
  fromRow =
    Auth <$> field <*> field

instance Select Auth

data Cat = Cat {cat_nameC :: Text, super_cat_idC :: CategoryId}
  deriving (Eq, Show)

instance FromRow Cat where
  fromRow =
    Cat <$> field <*> field

instance Select Cat

data Tag = Tag {tag_idT :: TagId, tag_nameT :: Text}
  deriving (Eq, Show)

instance FromRow Tag where
  fromRow =
    Tag <$> field <*> field

instance Select Tag

data Author = Author {author_idA :: AuthorId, author_infoA :: Text, user_idA :: UserId}
  deriving (Eq, Show)

instance FromRow Author where
  fromRow =
    Author <$> field <*> field <*> field

instance Select Author

data Comment = Comment {comment_idC :: CommentId, user_idC :: UserId, comment_textC :: Text}
  deriving (Eq, Show)

instance FromRow Comment where
  fromRow =
    Comment <$> field <*> field <*> field

instance Select Comment

data User = User {f_nameU :: Text, l_nameU :: Text, pic_idU :: PictureId, user_create_dateU :: Day}
  deriving (Eq, Show)

instance FromRow User where
  fromRow =
    User <$> field <*> field <*> field <*> field

instance Select User

data PostInfo = PostInfo {author_idPI :: AuthorId, author_infoPI :: Text, post_namePI :: Text, post_cat_idPI :: CategoryId, post_textPI :: Text, post_pic_idPI :: PictureId}
  deriving (Eq, Show)

instance FromRow PostInfo where
  fromRow =
    PostInfo <$> field <*> field <*> field <*> field <*> field <*> field

instance Select PostInfo

data Draft = Draft {draft_idD :: DraftId, author_infoD :: Text, post_idD :: PostId, draft_nameD :: Text, draft_cat_idD :: CategoryId, draft_textD :: Text, draft_pic_idD :: PictureId}
  deriving (Eq, Show)

instance FromRow Draft where
  fromRow =
    Draft <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance Select Draft

data Post = Post {post_idP :: PostId, author_idP :: AuthorId, author_infoP :: Text, user_idP :: UserId, post_nameP :: Text, post_create_dateU :: Day, post_cat_idP :: CategoryId, post_textP :: Text, post_pic_idP :: PictureId}
  deriving (Eq, Show)

instance FromRow Post where
  fromRow =
    Post <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance Select Post
