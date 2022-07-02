module Psql.Selecty where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Types

class (Show a, FromRow a) => Selecty a

data Auth = Auth {pwdAu :: Text, admBoolAu :: Bool}
  deriving (Eq, Show)

instance FromRow Auth where
  fromRow =
    Auth <$> field <*> field

instance Selecty Auth

data Cat = Cat {catNameC :: Text, superCatIdC :: CategoryId}
  deriving (Eq, Show)

instance FromRow Cat where
  fromRow =
    Cat <$> field <*> field

instance Selecty Cat

data Tag = Tag {tagIdT :: TagId, tagNameT :: Text}
  deriving (Eq, Show)

instance FromRow Tag where
  fromRow =
    Tag <$> field <*> field

instance Selecty Tag

data Author = Author
  { authorIdA :: AuthorId,
    authorInfoA :: Text,
    userIdA :: UserId
  }
  deriving (Eq, Show)

instance FromRow Author where
  fromRow =
    Author <$> field <*> field <*> field

instance Selecty Author

data Comment = Comment
  { commentIdC :: CommentId,
    userIdC :: UserId,
    commentTextC :: Text,
    postId :: PostId
  }
  deriving (Eq, Show)

instance FromRow Comment where
  fromRow =
    Comment <$> field <*> field <*> field <*> field

instance Selecty Comment

data User = User
  { firstNameU :: Text,
    lastNameU :: Text,
    picIdU :: PictureId,
    userCreateDateU :: Day
  }
  deriving (Eq, Show)

instance FromRow User where
  fromRow =
    User <$> field <*> field <*> field <*> field

instance Selecty User

data PostInfo = PostInfo
  { authorIdPI :: AuthorId,
    authorInfoPI :: Text,
    postNamePI :: Text,
    postCatIdPI :: CategoryId,
    postTextPI :: Text,
    postPicIdPI :: PictureId
  }
  deriving (Eq, Show)

instance FromRow PostInfo where
  fromRow =
    PostInfo <$> field <*> field <*> field <*> field <*> field <*> field

instance Selecty PostInfo

data Draft = Draft
  { draftIdDR :: DraftId,
    authorInfoDR :: Text,
    postIdDR :: PostId,
    draftNameDR :: Text,
    draftCatIdDR :: CategoryId,
    draftTextDR :: Text,
    draftPicIdDR :: PictureId
  }
  deriving (Eq, Show)

instance FromRow Draft where
  fromRow =
    Draft <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance Selecty Draft

data Post = Post
  { postIdPS :: PostId,
    authorIdPS :: AuthorId,
    authorInfoPS :: Text,
    userIdPS :: UserId,
    postNamePS :: Text,
    postCreateDatePS :: Day,
    postCatIdPS :: CategoryId,
    postTextPS :: Text,
    postPicIdPS :: PictureId
  }
  deriving (Eq, Show)

instance FromRow Post where
  fromRow =
    Post <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance Selecty Post
