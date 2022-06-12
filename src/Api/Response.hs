{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Api.Response where

import Data.Aeson (ToJSON (toEncoding, toJSON),genericToJSON,genericToEncoding)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Types
import Api.AesonOption (optionsEraseSuffix, optionsSnakeCase, optionsSnakeCasePreEraseSuffix)
import GHC.Generics (Generic)

data UserResponse = UserResponse
  { userId :: UserId,
    firstName :: Text,
    lastName :: Text,
    userPicId :: PictureId,
    userPicUrl :: Text,
    userCreateDate :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON UserResponse where
  toJSON = genericToJSON optionsSnakeCase
  toEncoding = genericToEncoding optionsSnakeCase

data UserTokenResponse = UserTokenResponse
  { tokenUTR :: Text,
    userIdUTR :: UserId,
    firstNameUTR :: Text,
    lastNameUTR :: Text,
    userPicIdUTR :: PictureId,
    userPicUrlUTR :: Text,
    userCreateDateUTR :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON UserTokenResponse where
  toJSON = genericToJSON $ optionsSnakeCasePreEraseSuffix "UTR"
  toEncoding = genericToEncoding $ optionsSnakeCasePreEraseSuffix "UTR"

newtype TokenResponse = TokenResponse {tokenTR :: Text} deriving (Eq, Show, Generic)

instance ToJSON TokenResponse where
  toJSON = genericToJSON $ optionsEraseSuffix "TR"
  toEncoding = genericToEncoding $ optionsEraseSuffix "TR"

data OkInfoResponse = OkInfoResponse {okOI :: Bool, infoOI :: Text} deriving (Eq, Show, Generic)

instance ToJSON OkInfoResponse where
  toJSON = genericToJSON $ optionsEraseSuffix "OI"
  toEncoding = genericToEncoding $ optionsEraseSuffix "OI"

data AuthorResponse = AuthorResponse
  { authorIdA :: AuthorId,
    authorInfoA :: Text,
    userIdA :: UserId
  }
  deriving (Eq, Show, Generic)

instance ToJSON AuthorResponse where
  toJSON = genericToJSON $ optionsSnakeCasePreEraseSuffix "A"
  toEncoding = genericToEncoding $ optionsSnakeCasePreEraseSuffix "A"

data CatResponse = Sub SubCatResponse | Super SuperCatResponse
  deriving (Eq, Show, Generic, ToJSON)

data SubCatResponse
  = SubCatResponse
      { categoryIdSCATR :: CategoryId,
        categoryNameSCATR :: Text,
        subCategoriesSCATR :: [CategoryId],
        superCategorySCATR :: CatResponse
      }
  deriving (Eq, Show, Generic)

instance ToJSON SubCatResponse where
  toJSON = genericToJSON $ optionsSnakeCasePreEraseSuffix "SCATR"
  toEncoding = genericToEncoding $ optionsSnakeCasePreEraseSuffix "SCATR"

data SuperCatResponse 
  = SuperCatResponse
      { categoryIdCATR :: CategoryId,
        categoryNameCATR :: Text,
        subCategoriesCATR :: [CategoryId]
      }
  deriving (Eq, Show, Generic)

instance ToJSON SuperCatResponse where
  toJSON = genericToJSON $ optionsSnakeCasePreEraseSuffix "CATR"
  toEncoding = genericToEncoding $ optionsSnakeCasePreEraseSuffix "CATR"

data PostIdOrNull = PostIdExist PostId | PostIdNull
  deriving (Eq, Generic)

instance Show PostIdOrNull where
  show (PostIdExist a) = show a
  show PostIdNull = "NULL"

instance ToJSON PostIdOrNull where
  toJSON (PostIdExist a) = toJSON a
  toJSON PostIdNull = toJSON ("NULL" :: Text)

data DraftResponse = DraftResponse
  { draftIdD :: DraftId,
    postIdD :: PostIdOrNull,
    authorD :: AuthorResponse,
    draftNameD :: Text,
    draftCategoryD :: CatResponse,
    draftTextD :: Text,
    draftMainPicIdD :: PictureId,
    draftMainPicUrlD :: Text,
    draftPicsD :: [PicIdUrl],
    draftTagsD :: [TagResponse]
  }
  deriving (Eq, Show, Generic)

instance ToJSON DraftResponse where
  toJSON = genericToJSON $ optionsSnakeCasePreEraseSuffix "D"
  toEncoding = genericToEncoding $ optionsSnakeCasePreEraseSuffix "D"

data PicIdUrl = PicIdUrl
  { picIdPU :: PictureId,
    picUrlPU :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON PicIdUrl where
  toJSON = genericToJSON $ optionsSnakeCasePreEraseSuffix "PU"
  toEncoding = genericToEncoding $ optionsSnakeCasePreEraseSuffix "PU"

data DraftsResponse = DraftsResponse
  { page :: Page,
    drafts :: [DraftResponse]
  }
  deriving (Eq, Show, ToJSON, Generic)

data PostResponse = PostResponse
  { postIdP :: PostId,
    authorP :: AuthorResponse,
    postNameP :: Text,
    postCreateDateP :: Day,
    postCategoryP :: CatResponse,
    postTextP :: Text,
    postMainPicIdP :: PictureId,
    postMainPicUrlP :: Text,
    postPicsP :: [PicIdUrl],
    postTagsP :: [TagResponse]
  }
  deriving (Eq, Show, Generic)

instance ToJSON PostResponse where
  toJSON = genericToJSON $ optionsSnakeCasePreEraseSuffix "P"
  toEncoding = genericToEncoding $ optionsSnakeCasePreEraseSuffix "P"

data PostsResponse = PostsResponse
  { pageP :: Page,
    postsP :: [PostResponse]
  }
  deriving (Eq, Show, Generic)

instance ToJSON PostsResponse where
  toJSON = genericToJSON $ optionsSnakeCasePreEraseSuffix "P"
  toEncoding = genericToEncoding $ optionsSnakeCasePreEraseSuffix "P"

data TagResponse = TagResponse
  { tagIdTR :: TagId,
    tagNameTR :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON TagResponse where
  toJSON = genericToJSON $ optionsSnakeCasePreEraseSuffix "TR"
  toEncoding = genericToEncoding $ optionsSnakeCasePreEraseSuffix "TR"

data CommentResponse = CommentResponse
  { commentIdCR :: CommentId,
    commentTextCR :: Text,
    userIdCR :: UserId,
    postIdCR :: PostId
  }
  deriving (Eq, Show, Generic)

instance ToJSON CommentResponse where
  toJSON = genericToJSON $ optionsSnakeCasePreEraseSuffix "CR"
  toEncoding = genericToEncoding $ optionsSnakeCasePreEraseSuffix "CR"

data CommentIdTextUserResponse = CommentIdTextUserResponse
  { commentIdCTR :: CommentId,
    commentTextCTR :: Text,
    userIdCTR :: UserId
  }
  deriving (Eq, Show, Generic)

instance ToJSON CommentIdTextUserResponse where
  toJSON = genericToJSON $ optionsSnakeCasePreEraseSuffix "CTR"
  toEncoding = genericToEncoding $ optionsSnakeCasePreEraseSuffix "CTR"

data CommentsResponse = CommentsResponse
  { pageCSR :: Page,
    postIdCSR :: PostId,
    commentsCSR :: [CommentIdTextUserResponse]
  }
  deriving (Eq, Show, Generic)

instance ToJSON CommentsResponse where
  toJSON = genericToJSON $ optionsSnakeCasePreEraseSuffix "CSR"
  toEncoding = genericToEncoding $ optionsSnakeCasePreEraseSuffix "CSR"
