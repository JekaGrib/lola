{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}


module Api.Request where


import           Data.Aeson                     (FromJSON(parseJSON),withObject,(.:))
import           Data.Text                      ( Text )


data DraftRequest = DraftRequest {
      tokenDR :: Text
    , draft_name    :: Text
    , draft_cat_id :: Integer
    , draft_textDR  :: Text
    , draft_main_pic_id :: Integer
    , draft_pics_ids :: [Integer]
    , draft_tags_ids :: [Integer]
    } deriving (Eq,Show)

instance FromJSON DraftRequest where
    parseJSON = withObject "DraftRequest" $ \v -> DraftRequest
        <$> v .: "token"
        <*> v .: "draft_name"
        <*> v .: "draft_category_id"
        <*> v .: "draft_text"
        <*> v .: "draft_main_pic_id"
        <*> v .: "draft_pics_ids"
        <*> v .: "draft_tags_ids" 





