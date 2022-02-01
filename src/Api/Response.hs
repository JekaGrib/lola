{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}


module Api.Response where


import           Data.Aeson                     (ToJSON(toJSON,toEncoding),object,pairs,(.=))
import           Data.Text                      ( unpack, Text )

data UserResponse = UserResponse {
      user_id      :: Integer
    , first_name   :: Text
    , last_name    :: Text
    , user_pic_id  :: Integer
    , user_pic_url :: Text
    , user_create_date :: Text
    } deriving (Eq,Show)

instance ToJSON UserResponse where
    toJSON (UserResponse a b c d e f ) =
        object ["user_id" .= a, "first_name" .= b, "last_name" .= c, "user_pic_id" .= d, "user_pic_url" .= e, "user_create_date" .= f]
    toEncoding (UserResponse a b c d e f) =
        pairs ("user_id" .= a <> "first_name" .= b <> "last_name" .= c <> "user_pic_id" .= d <> "user_pic_url" .= e <> "user_create_date" .= f)

data UserTokenResponse = UserTokenResponse {
    tokenUTR :: Text
    , user_idUTR      :: Integer
    , first_nameUTR   :: Text
    , last_nameUTR   :: Text
    , user_pic_idUTR  :: Integer
    , user_pic_urlUTR :: Text
    , user_create_dateUTR :: Text
    } deriving (Eq,Show)

instance ToJSON UserTokenResponse where
    toJSON (UserTokenResponse a b c d e f g) =
        object ["token" .= a, "user_id" .= b, "first_name" .= c, "last_name" .= d, "user_pic_id" .= e, "user_pic_url" .= f, "user_create_date" .= g]
    toEncoding (UserTokenResponse a b c d e f g) =
        pairs ("token" .= a <> "user_id" .= b <> "first_name" .= c <> "last_name" .= d <> "user_pic_id" .= e <> "user_pic_url" .= f <> "user_create_date" .= g)

data TokenResponse = TokenResponse {tokenTR :: Text} deriving (Eq,Show)

instance ToJSON TokenResponse where
    toJSON (TokenResponse a) =
        object ["token" .= a]
    toEncoding (TokenResponse a) =
        pairs ("token" .= a )

data OkResponse = OkResponse {ok :: Bool} deriving (Eq,Show)

instance ToJSON OkResponse where
    toJSON (OkResponse a) =
        object ["ok" .= a]
    toEncoding (OkResponse a) =
        pairs ("ok" .= a )


data OkInfoResponse = OkInfoResponse {ok7 :: Bool, info7 :: Text} deriving (Eq,Show)

instance ToJSON OkInfoResponse where
    toJSON (OkInfoResponse a b) =
        object ["ok" .= a, "info" .= b]
    toEncoding (OkInfoResponse a b) =
        pairs ("ok" .= a <> "info" .= b )


data AuthorResponse = AuthorResponse {
      author_id    :: Integer
    , author_info  :: Text
    , auth_user_id :: Integer
    } deriving (Eq,Show)

instance ToJSON AuthorResponse where
    toJSON (AuthorResponse a b c ) =
        object ["author_id" .= a, "author_info" .= b, "user_id" .= c]
    toEncoding (AuthorResponse a b c ) =
        pairs ( "author_id" .= a  <> "author_info" .= b <> "user_id" .= c)


data CatResponse 
  = SubCatResponse {
      subCat_id                :: Integer
    , subCat_name              :: Text
    , one_level_sub_categories :: [Integer]
    , super_category           :: CatResponse
    } 
  | CatResponse {
      cat_id             :: Integer
    , cat_name           :: Text
    , one_level_sub_cats :: [Integer]
    , super_cat          :: Text
    } deriving (Eq,Show)

instance ToJSON CatResponse where
    toJSON (CatResponse a b c d) =
        object ["category_id" .= a, "category_name" .= b, "sub_categories" .= c, "super_category" .= d]
    toJSON (SubCatResponse a b c d) =
        object ["category_id" .= a, "category_name" .= b, "sub_categories" .= c, "super_category" .= d]
    toEncoding (CatResponse a b c d) =
        pairs ( "category_id" .= a <> "category_name" .= b <> "sub_categories"  .= c <> "super_category" .= d)
    toEncoding (SubCatResponse a b c d) =
        pairs ( "category_id" .= a <> "category_name" .= b <> "sub_categories"  .= c <> "super_category" .= d)



data PostId = PostInteger Integer | PostText Text 
  deriving Eq

instance Show PostId where
  show (PostInteger a) = show a
  show (PostText a) = unpack a

instance ToJSON PostId where
  toJSON (PostInteger a) = toJSON a
  toJSON (PostText a) = toJSON a
  

data DraftResponse = DraftResponse {
      draft_id2      :: Integer
    , post_id2      :: PostId
    , author2   :: AuthorResponse
    , draft_name2    :: Text
    , draft_cat2 :: CatResponse
    , draft_text2  :: Text
    , draft_main_pic_id2  :: Integer
    , draft_main_pic_url2 :: Text
    , draft_pics2 :: [PicIdUrl]
    , draft_tags2 ::  [TagResponse]
    } deriving (Eq,Show)

instance ToJSON DraftResponse where
    toJSON (DraftResponse a b c d e f g h i j) =
        object ["draft_id" .= a, "post_id" .= b, "author" .= c, "draft_name" .= d, "draft_category" .= e, "draft_text" .= f, "draft_main_pic_id" .= g, "draft_main_pic_url" .= h, "draft_pics" .= i, "draft_tags" .= j]
    toEncoding (DraftResponse a b c d e f g h i j) =
        pairs ("draft_id" .= a <> "post_id" .= b <> "author" .= c <> "draft_name" .= d <> "draft_category" .= e <> "draft_text" .= f <> "draft_main_pic_id" .= g <> "draft_main_pic_url" .= h <> "draft_pics" .= i <> "draft_tags" .= j)


data PicIdUrl = PicIdUrl {
      pic_id   :: Integer
    , pic_url2 :: Text
    } deriving (Eq,Show)

instance ToJSON PicIdUrl where
    toJSON (PicIdUrl a b ) =
        object ["pic_id" .= a, "pic_url" .= b]
    toEncoding (PicIdUrl a b ) =
        pairs ( "pic_id" .= a <> "pic_url" .= b )


data DraftsResponse = DraftsResponse {
      page9     :: Integer
    , drafts9 :: [DraftResponse]
    } deriving (Eq,Show)

instance ToJSON DraftsResponse where
    toJSON (DraftsResponse a b ) =
        object ["page" .= a, "drafts" .= b]
    toEncoding (DraftsResponse a b ) =
        pairs ( "page" .= a <> "drafts" .= b )
  

data PostResponse = PostResponse {
      post_id      :: Integer
    , author4   :: AuthorResponse
    , post_name    :: Text
    , post_create_date :: Text
    , post_cat     :: CatResponse
    , post_text    :: Text
    , post_main_pic_id  :: Integer
    , post_main_pic_url :: Text
    , post_pics :: [PicIdUrl]
    , post_tags :: [TagResponse]
    } deriving (Eq,Show)

instance ToJSON PostResponse where
    toJSON (PostResponse a b c d e f g h i j) =
        object ["post_id" .= a, "author" .= b, "post_name" .= c, "post_create_date" .= d, "post_category" .= e, "post_text" .= f, "post_main_pic_id" .= g, "post_main_pic_url" .= h, "post_pics" .= i, "post_tags" .= j]
    toEncoding (PostResponse a b c d e f g h i j) =
        pairs ("post_id" .= a <> "author" .= b  <> "post_name" .= c <> "post_create_date" .= d <> "post_category" .= e <> "post_text" .= f <> "post_main_pic_id" .= g <> "post_main_pic_url" .= h <> "post_pics" .= i <> "post_tags" .= j)


data PostsResponse = PostsResponse {
      page10     :: Integer
    , posts10 :: [PostResponse]
    } deriving (Eq,Show)

instance ToJSON PostsResponse where
    toJSON (PostsResponse a b ) =
        object ["page" .= a, "posts" .= b]
    toEncoding (PostsResponse a b ) =
        pairs ( "page" .= a <> "posts" .= b )


data TagResponse = TagResponse {
      tag_id   :: Integer
    , tag_name :: Text
    } deriving (Eq,Show)

instance ToJSON TagResponse where
    toJSON (TagResponse a b ) =
        object ["tag_id" .= a, "tag_name" .= b]
    toEncoding (TagResponse a b ) =
        pairs ( "tag_id" .= a <> "tag_name" .= b )


data CommentResponse = CommentResponse {
      comment_id   :: Integer
    , comment_text :: Text
    , post_id6   :: Integer
    , user_id6   :: Integer
    } deriving (Eq,Show)

instance ToJSON CommentResponse where
    toJSON (CommentResponse a b c d) =
        object ["comment_id" .= a, "comment_text" .= b, "post_id" .= c, "user_id" .= d]
    toEncoding (CommentResponse a b c d) =
        pairs ( "comment_id" .= a <> "comment_text" .= b <> "post_id" .= c <> "user_id" .= d )


data CommentIdTextUserResponse = CommentIdTextUserResponse {
      comment_id8   :: Integer
    , comment_text8 :: Text
    , user_id8   :: Integer
    } deriving (Eq,Show)

instance ToJSON CommentIdTextUserResponse where
    toJSON (CommentIdTextUserResponse a b c) =
        object ["comment_id" .= a, "comment_text" .= b, "user_id" .= c]
    toEncoding (CommentIdTextUserResponse a b c) =
        pairs ( "comment_id" .= a <> "comment_text" .= b <> "user_id" .= c )


data CommentsResponse = CommentsResponse {
      page     :: Integer
    , post_id9 :: Integer
    , comments :: [CommentIdTextUserResponse]
    } deriving (Eq,Show)

instance ToJSON CommentsResponse where
    toJSON (CommentsResponse a b c) =
        object ["page" .= a, "post_id" .= b, "comments" .= c]
    toEncoding (CommentsResponse a b c) =
        pairs ( "page" .= a <> "post_id" .= b <> "comments" .= c )

