{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Api.Response where

import Data.Aeson ((.=), ToJSON (toEncoding, toJSON), object, pairs)
import Data.Text (Text)
import Types
import Data.Time.Calendar ( Day)


data UserResponse = UserResponse
  { user_id :: UserId,
    first_name :: Text,
    last_name :: Text,
    user_pic_id :: PictureId,
    user_pic_url :: Text,
    user_create_date :: Day
  }
  deriving (Eq, Show)

instance ToJSON UserResponse where
  toJSON (UserResponse a b c d e f) =
    object ["user_id" .= a, "first_name" .= b, "last_name" .= c, "user_pic_id" .= d, "user_pic_url" .= e, "user_create_date" .= f]
  toEncoding (UserResponse a b c d e f) =
    pairs ("user_id" .= a <> "first_name" .= b <> "last_name" .= c <> "user_pic_id" .= d <> "user_pic_url" .= e <> "user_create_date" .= f)

data UserTokenResponse = UserTokenResponse
  { tokenUTR :: Text,
    user_idUTR :: UserId,
    first_nameUTR :: Text,
    last_nameUTR :: Text,
    user_pic_idUTR :: PictureId,
    user_pic_urlUTR :: Text,
    user_create_dateUTR :: Day
  }
  deriving (Eq, Show)

instance ToJSON UserTokenResponse where
  toJSON (UserTokenResponse a b c d e f g) =
    object ["token" .= a, "user_id" .= b, "first_name" .= c, "last_name" .= d, "user_pic_id" .= e, "user_pic_url" .= f, "user_create_date" .= g]
  toEncoding (UserTokenResponse a b c d e f g) =
    pairs ("token" .= a <> "user_id" .= b <> "first_name" .= c <> "last_name" .= d <> "user_pic_id" .= e <> "user_pic_url" .= f <> "user_create_date" .= g)

newtype TokenResponse = TokenResponse {tokenTR :: Text} deriving (Eq, Show)

instance ToJSON TokenResponse where
  toJSON (TokenResponse a) =
    object ["token" .= a]
  toEncoding (TokenResponse a) =
    pairs ("token" .= a)



data OkInfoResponse = OkInfoResponse {ok7 :: Bool, info7 :: Text} deriving (Eq, Show)

instance ToJSON OkInfoResponse where
  toJSON (OkInfoResponse a b) =
    object ["ok" .= a, "info" .= b]
  toEncoding (OkInfoResponse a b) =
    pairs ("ok" .= a <> "info" .= b)

data AuthorResponse = AuthorResponse
  { author_id :: AuthorId,
    author_info :: Text,
    auth_user_id :: UserId
  }
  deriving (Eq, Show)

instance ToJSON AuthorResponse where
  toJSON (AuthorResponse a b c) =
    object ["author_id" .= a, "author_info" .= b, "user_id" .= c]
  toEncoding (AuthorResponse a b c) =
    pairs ("author_id" .= a <> "author_info" .= b <> "user_id" .= c)

data CatResponse
  = SubCatResponse
      { subCat_id :: CategoryId,
        subCat_name :: Text,
        one_level_sub_categories :: [CategoryId],
        super_category :: CatResponse
      }
  | CatResponse
      { cat_id :: CategoryId,
        cat_name :: Text,
        one_level_sub_cats :: [CategoryId]
      }
  deriving (Eq, Show)

instance ToJSON CatResponse where
  toJSON (CatResponse a b c) =
    object ["category_id" .= a, "category_name" .= b, "sub_categories" .= c]
  toJSON (SubCatResponse a b c d) =
    object ["category_id" .= a, "category_name" .= b, "sub_categories" .= c, "super_category" .= d]
  toEncoding (CatResponse a b c) =
    pairs ("category_id" .= a <> "category_name" .= b <> "sub_categories" .= c)
  toEncoding (SubCatResponse a b c d) =
    pairs ("category_id" .= a <> "category_name" .= b <> "sub_categories" .= c <> "super_category" .= d)

data PostIdOrNull = PostIdExist PostId | PostIdNull
  deriving (Eq)

instance Show PostIdOrNull where
  show (PostIdExist a) = show a
  show PostIdNull = "NULL"

instance ToJSON PostIdOrNull where
  toJSON (PostIdExist a) = toJSON a
  toJSON PostIdNull = toJSON ("NULL" :: Text)

data DraftResponse = DraftResponse
  { draft_id2 :: DraftId,
    post_id2 :: PostIdOrNull,
    author2 :: AuthorResponse,
    draft_name2 :: Text,
    draft_cat2 :: CatResponse,
    draft_text2 :: Text,
    draft_main_pic_id2 :: PictureId,
    draft_main_pic_url2 :: Text,
    draft_pics2 :: [PicIdUrl],
    draft_tags2 :: [TagResponse]
  }
  deriving (Eq, Show)

instance ToJSON DraftResponse where
  toJSON (DraftResponse a b c d e f g h i j) =
    object ["draft_id" .= a, "post_id" .= b, "author" .= c, "draft_name" .= d, "draft_category" .= e, "draft_text" .= f, "draft_main_pic_id" .= g, "draft_main_pic_url" .= h, "draft_pics" .= i, "draft_tags" .= j]
  toEncoding (DraftResponse a b c d e f g h i j) =
    pairs ("draft_id" .= a <> "post_id" .= b <> "author" .= c <> "draft_name" .= d <> "draft_category" .= e <> "draft_text" .= f <> "draft_main_pic_id" .= g <> "draft_main_pic_url" .= h <> "draft_pics" .= i <> "draft_tags" .= j)

data PicIdUrl = PicIdUrl
  { pic_idPU :: PictureId,
    pic_urlPU :: Text
  }
  deriving (Eq, Show)

instance ToJSON PicIdUrl where
  toJSON (PicIdUrl a b) =
    object ["pic_id" .= a, "pic_url" .= b]
  toEncoding (PicIdUrl a b) =
    pairs ("pic_id" .= a <> "pic_url" .= b)

data DraftsResponse = DraftsResponse
  { page9 :: Page,
    drafts9 :: [DraftResponse]
  }
  deriving (Eq, Show)

instance ToJSON DraftsResponse where
  toJSON (DraftsResponse a b) =
    object ["page" .= a, "drafts" .= b]
  toEncoding (DraftsResponse a b) =
    pairs ("page" .= a <> "drafts" .= b)

data PostResponse = PostResponse
  { post_id :: PostId,
    author4 :: AuthorResponse,
    post_name :: Text,
    post_create_date :: Day,
    post_cat :: CatResponse,
    post_text :: Text,
    post_main_pic_id :: PictureId,
    post_main_pic_url :: Text,
    post_pics :: [PicIdUrl],
    post_tags :: [TagResponse]
  }
  deriving (Eq, Show)

instance ToJSON PostResponse where
  toJSON (PostResponse a b c d e f g h i j) =
    object ["post_id" .= a, "author" .= b, "post_name" .= c, "post_create_date" .= d, "post_category" .= e, "post_text" .= f, "post_main_pic_id" .= g, "post_main_pic_url" .= h, "post_pics" .= i, "post_tags" .= j]
  toEncoding (PostResponse a b c d e f g h i j) =
    pairs ("post_id" .= a <> "author" .= b <> "post_name" .= c <> "post_create_date" .= d <> "post_category" .= e <> "post_text" .= f <> "post_main_pic_id" .= g <> "post_main_pic_url" .= h <> "post_pics" .= i <> "post_tags" .= j)

data PostsResponse = PostsResponse
  { page10 :: Page,
    posts10 :: [PostResponse]
  }
  deriving (Eq, Show)

instance ToJSON PostsResponse where
  toJSON (PostsResponse a b) =
    object ["page" .= a, "posts" .= b]
  toEncoding (PostsResponse a b) =
    pairs ("page" .= a <> "posts" .= b)

data TagResponse = TagResponse
  { tag_idTR :: TagId,
    tag_nameTR :: Text
  }
  deriving (Eq, Show)

instance ToJSON TagResponse where
  toJSON (TagResponse a b) =
    object ["tag_id" .= a, "tag_name" .= b]
  toEncoding (TagResponse a b) =
    pairs ("tag_id" .= a <> "tag_name" .= b)

data CommentResponse = CommentResponse
  { comment_idCR :: CommentId,
    comment_textCR :: Text,
    user_idCR :: UserId,
    post_idCR :: PostId
  }
  deriving (Eq, Show)

instance ToJSON CommentResponse where
  toJSON (CommentResponse a b c d) =
    object ["comment_id" .= a, "comment_text" .= b, "user_id" .= c, "post_id" .= d]
  toEncoding (CommentResponse a b c d) =
    pairs ("comment_id" .= a <> "comment_text" .= b <> "user_id" .= c <> "post_id" .= d)

data CommentIdTextUserResponse = CommentIdTextUserResponse
  { comment_id8 :: CommentId,
    comment_text8 :: Text,
    user_id8 :: UserId
  }
  deriving (Eq, Show)

instance ToJSON CommentIdTextUserResponse where
  toJSON (CommentIdTextUserResponse a b c) =
    object ["comment_id" .= a, "comment_text" .= b, "user_id" .= c]
  toEncoding (CommentIdTextUserResponse a b c) =
    pairs ("comment_id" .= a <> "comment_text" .= b <> "user_id" .= c)

data CommentsResponse = CommentsResponse
  { pageCR :: Page,
    post_id9 :: PostId,
    comments :: [CommentIdTextUserResponse]
  }
  deriving (Eq, Show)

instance ToJSON CommentsResponse where
  toJSON (CommentsResponse a b c) =
    object ["page" .= a, "post_id" .= b, "comments" .= c]
  toEncoding (CommentsResponse a b c) =
    pairs ("page" .= a <> "post_id" .= b <> "comments" .= c)
