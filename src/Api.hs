{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}


module Api where


import           Data.Aeson
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
    toJSON (OkInfoResponse ok info) =
        object ["ok" .= ok, "info" .= info]
    toEncoding (OkInfoResponse ok info) =
        pairs ("ok" .= ok <> "info" .= info )


data AuthorResponse = AuthorResponse {
      author_id    :: Integer
    , author_info  :: Text
    , auth_user_id :: Integer
    } deriving (Eq,Show)

instance ToJSON AuthorResponse where
    toJSON (AuthorResponse author_id author_info auth_user_id ) =
        object ["author_id" .= author_id, "author_info" .= author_info, "user_id" .= auth_user_id]
    toEncoding (AuthorResponse author_id author_info auth_user_id ) =
        pairs ( "author_id" .= author_id  <> "author_info" .= author_info <> "user_id" .= auth_user_id)


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
    toJSON (CatResponse cat_id cat_name all_sub_cats super_cat) =
        object ["category_id" .= cat_id, "category_name" .= cat_name, "sub_categories" .= all_sub_cats, "super_category" .= super_cat]
    toJSON (SubCatResponse cat_id cat_name all_sub_cats super_cat) =
        object ["category_id" .= cat_id, "category_name" .= cat_name, "sub_categories" .= all_sub_cats, "super_category" .= super_cat]
    toEncoding (CatResponse cat_id cat_name all_sub_cats super_cat) =
        pairs ( "category_id" .= cat_id <> "category_name" .= cat_name <> "sub_categories"  .= all_sub_cats <> "super_category" .= super_cat)
    toEncoding (SubCatResponse cat_id cat_name all_sub_cats super_cat) =
        pairs ( "category_id" .= cat_id <> "category_name" .= cat_name <> "sub_categories"  .= all_sub_cats <> "super_category" .= super_cat)


data DraftRequest = DraftRequest {
      tokenDR :: Text
    , draft_name    :: Text
    , draft_cat_id :: Integer
    , draft_textDR  :: Text
    , draft_main_pic_url :: Text
    , draft_pics_urls :: [Text]
    , draft_tags_ids :: [Integer]
    } deriving (Eq,Show)

instance FromJSON DraftRequest where
    parseJSON (Object v) = DraftRequest
        <$> v .: "token"
        <*> v .: "draft_name"
        <*> v .: "draft_category_id"
        <*> v .: "draft_text"
        <*> v .: "draft_main_pic_url"
        <*> v .: "draft_pics_urls"
        <*> v .: "draft_tags_ids" 

instance ToJSON DraftRequest where
    toJSON (DraftRequest a b c d e f g ) =
        object ["token" .= a, "draft_name" .= b, "draft_category_id" .= c, "draft_text" .= d, "draft_main_pic_url" .= e, "draft_pics_urls" .= f, "draft_tags_ids" .= g]
    toEncoding (DraftRequest a b c d e f g) =
        pairs ("token" .= a <> "draft_name" .= b <> "draft_category_id" .= c <> "draft_text" .= d <> "draft_main_pic_url" .= e <> "draft_pics_urls" .= f <> "draft_tags_ids" .= g)


{-data PicUrl = PicUrl {
      pic_url :: Text
    } deriving (Eq,Show)

instance FromJSON PicUrl where
    parseJSON (Object v) = PicUrl
        <$> v .: "pic_url"

instance ToJSON PicUrl where
    toJSON (PicUrl pic_url ) =
        object ["pic_url" .= pic_url]
    toEncoding (PicUrl pic_url ) =
        pairs ( "pic_url" .= pic_url )


data TagId = TagId {
      tag_id3 :: Integer
    } deriving (Eq,Show)

instance ToJSON TagId where
    toJSON (TagId tag_id3 ) =
        object ["tag_id" .= tag_id3]
    toEncoding (TagId tag_id3 ) =
        pairs ( "tag_id" .= tag_id3 )

instance FromJSON TagId where
    parseJSON (Object v) = TagId
        <$> v .: "tag_id"
-}

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
    toJSON (DraftResponse draft_id post_id author draft_name draft_cat draft_text draft_main_pic_id draft_main_pic_url draft_pics draft_tags) =
        object ["draft_id" .= draft_id, "post_id" .= post_id, "author" .= author, "draft_name" .= draft_name, "draft_category" .= draft_cat, "draft_text" .= draft_text, "draft_main_pic_id" .= draft_main_pic_id, "draft_main_pic_url" .= draft_main_pic_url, "draft_pics" .= draft_pics, "draft_tags" .= draft_tags]
    toEncoding (DraftResponse draft_id post_id author draft_name draft_cat draft_text draft_main_pic_id draft_main_pic_url draft_pics draft_tags ) =
        pairs ("draft_id" .= draft_id <> "post_id" .= post_id <> "author" .= author <> "draft_name" .= draft_name <> "draft_category" .= draft_cat <> "draft_text" .= draft_text <> "draft_main_pic_id" .= draft_main_pic_id <> "draft_main_pic_url" .= draft_main_pic_url <> "draft_pics" .= draft_pics <> "draft_tags" .= draft_tags)


data PicIdUrl = PicIdUrl {
      pic_id   :: Integer
    , pic_url2 :: Text
    } deriving (Eq,Show)

instance ToJSON PicIdUrl where
    toJSON (PicIdUrl pic_id2 pic_url ) =
        object ["pic_id" .= pic_id2, "pic_url" .= pic_url]
    toEncoding (PicIdUrl pic_id2 pic_url ) =
        pairs ( "pic_id" .= pic_id2 <> "pic_url" .= pic_url )


data DraftsResponse = DraftsResponse {
      page9     :: Integer
    , drafts9 :: [DraftResponse]
    } deriving (Eq,Show)

instance ToJSON DraftsResponse where
    toJSON (DraftsResponse page drafts ) =
        object ["page" .= page, "drafts" .= drafts]
    toEncoding (DraftsResponse page drafts ) =
        pairs ( "page" .= page <> "drafts" .= drafts )
  

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
    toJSON (PostResponse post_id author4 post_name post_create_date post_cat post_text post_main_pic_id post_main_pic_url post_pics post_tags) =
        object ["post_id" .= post_id, "author" .= author4, "post_name" .= post_name, "post_create_date" .= post_create_date, "post_category" .= post_cat, "post_text" .= post_text, "post_main_pic_id" .= post_main_pic_id, "post_main_pic_url" .= post_main_pic_url, "post_pics" .= post_pics, "post_tags" .= post_tags]
    toEncoding (PostResponse post_id author4 post_name post_create_date post_cat post_text post_main_pic_id post_main_pic_url post_pics post_tags) =
        pairs ("post_id" .= post_id <> "author" .= author4  <> "post_name" .= post_name <> "post_create_date" .= post_create_date <> "post_category" .= post_cat <> "post_text" .= post_text <> "post_main_pic_id" .= post_main_pic_id <> "post_main_pic_url" .= post_main_pic_url <> "post_pics" .= post_pics <> "post_tags" .= post_tags)


data PostsResponse = PostsResponse {
      page10     :: Integer
    , posts10 :: [PostResponse]
    } deriving (Eq,Show)

instance ToJSON PostsResponse where
    toJSON (PostsResponse page posts ) =
        object ["page" .= page, "posts" .= posts]
    toEncoding (PostsResponse page posts ) =
        pairs ( "page" .= page <> "posts" .= posts )


data TagResponse = TagResponse {
      tag_id   :: Integer
    , tag_name :: Text
    } deriving (Eq,Show)

instance ToJSON TagResponse where
    toJSON (TagResponse tag_id tag_name ) =
        object ["tag_id" .= tag_id, "tag_name" .= tag_name]
    toEncoding (TagResponse tag_id tag_name ) =
        pairs ( "tag_id" .= tag_id <> "tag_name" .= tag_name )


data CommentResponse = CommentResponse {
      comment_id   :: Integer
    , comment_text :: Text
    , post_id6   :: Integer
    , user_id6   :: Integer
    } deriving (Eq,Show)

instance ToJSON CommentResponse where
    toJSON (CommentResponse comment_id comment_text post_id user_id) =
        object ["comment_id" .= comment_id, "comment_text" .= comment_text, "post_id" .= post_id, "user_id" .= user_id]
    toEncoding (CommentResponse comment_id comment_text post_id user_id) =
        pairs ( "comment_id" .= comment_id <> "comment_text" .= comment_text <> "post_id" .= post_id <> "user_id" .= user_id )


data CommentIdTextUserResponse = CommentIdTextUserResponse {
      comment_id8   :: Integer
    , comment_text8 :: Text
    , user_id8   :: Integer
    } deriving (Eq,Show)

instance ToJSON CommentIdTextUserResponse where
    toJSON (CommentIdTextUserResponse comment_id comment_text user_id) =
        object ["comment_id" .= comment_id, "comment_text" .= comment_text, "user_id" .= user_id]
    toEncoding (CommentIdTextUserResponse comment_id comment_text user_id) =
        pairs ( "comment_id" .= comment_id <> "comment_text" .= comment_text <> "user_id" .= user_id )


data CommentsResponse = CommentsResponse {
      page     :: Integer
    , post_id9 :: Integer
    , comments :: [CommentIdTextUserResponse]
    } deriving (Eq,Show)

instance ToJSON CommentsResponse where
    toJSON (CommentsResponse page post_id comments) =
        object ["page" .= page, "post_id" .= post_id, "comments" .= comments]
    toEncoding (CommentsResponse page post_id comments) =
        pairs ( "page" .= page <> "post_id" .= post_id <> "comments" .= comments )

