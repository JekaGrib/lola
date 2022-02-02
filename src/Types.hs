{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE FlexibleInstances #-}


module Types where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text)
import           Data.Time.Calendar             ( Day)



type UserId = Integer
type PostId = Integer
type CommentId = Integer

class (Show a,FromRow a) => Select a


data TwoIds = TwoIds   {id_1 :: Integer, id_2 :: Integer}
    deriving (Eq,Show)

data Auth = Auth  {pwdAu :: Text, admBoolAu :: Bool}
    deriving (Eq,Show)
data Cat = Cat      {cat_nameC :: Text, super_cat_idC :: Integer}
    deriving (Eq,Show)
data Tag = Tag      {tag_idT :: Integer, tag_nameT :: Text}
    deriving (Eq,Show)
data Author = Author   {author_idA :: Integer, author_infoA :: Text, user_idA :: Integer}
    deriving (Eq,Show)
data Comment = Comment  {comment_idC :: Integer, user_idC :: Integer, comment_textC :: Text}
    deriving (Eq,Show)
data User = User     {f_nameU :: Text, l_nameU :: Text, pic_idU :: Integer, user_create_dateU :: Day}
    deriving (Eq,Show)
data PostInfo = PostInfo {author_idPI :: Integer, author_infoPI :: Text, post_namePI :: Text, post_cat_idPI :: Integer, post_textPI :: Text, post_pic_idPI :: Integer}
    deriving (Eq,Show)
data Draft = Draft {draft_idD :: Integer, author_infoD :: Text, post_idD :: Integer, draft_nameD :: Text, draft_cat_idD :: Integer, draft_textD :: Text, draft_pic_idD :: Integer}
    deriving (Eq,Show)
data Post  = Post  {post_idP :: Integer, author_idP :: Integer, author_infoP :: Text, user_idP :: Integer, post_nameP :: Text, post_create_dateU :: Day, post_cat_idP :: Integer, post_textP :: Text, post_pic_idP :: Integer}
    deriving (Eq,Show)

instance Select (Only Integer)

instance Select (Only Day)

instance Select (Only Text)

instance Select (Only (Binary ByteString))

  
instance FromRow Post where
  fromRow = 
    Post <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance Select Post

instance FromRow Draft where
  fromRow = 
    Draft <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance Select Draft

instance FromRow PostInfo where
  fromRow = 
    PostInfo <$> field <*> field <*> field <*> field <*> field <*> field

instance Select PostInfo

instance FromRow User where
  fromRow = 
    User <$> field <*> field <*> field <*> field

instance Select User

instance FromRow Comment where
  fromRow = 
    Comment <$> field <*> field <*> field

instance Select Comment

instance FromRow Author where
  fromRow = 
    Author <$> field <*> field <*> field

instance Select Author

instance FromRow Tag where
  fromRow = 
    Tag <$> field <*> field

instance Select Tag

instance FromRow Cat where
  fromRow = 
    Cat <$> field <*> field

instance Select Cat

instance FromRow Auth where
  fromRow = 
    Auth <$> field <*> field

instance Select Auth

instance FromRow TwoIds where
  fromRow = 
    TwoIds <$> field <*> field
    
instance Select TwoIds
    

