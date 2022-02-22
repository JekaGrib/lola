{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Api.Request where

import Data.Aeson ((.:), FromJSON (parseJSON), withObject)
import Data.Text (Text)
import Types

data DraftRequest = DraftRequest
  { tokenDR :: Text,
    draft_name :: Text,
    draft_cat_id :: CategoryId,
    draft_textDR :: Text,
    draft_main_pic_id :: PictureId,
    draft_pics_ids :: [PictureId],
    draft_tags_ids :: [TagId]
  }
  deriving (Eq, Show)

instance FromJSON DraftRequest where
  parseJSON = withObject "DraftRequest" $ \v ->
    DraftRequest
      <$> v .: "token"
      <*> v .: "draft_name"
      <*> v .: "draft_category_id"
      <*> v .: "draft_text"
      <*> v .: "draft_main_pic_id"
      <*> v .: "draft_pics_ids"
      <*> v .: "draft_tags_ids"
