{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Picture.Types where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Types

data PicMock
  = SelectPicBS PictureId
  | InsertReturnPicBS ByteString
  | GoToUrl Text
  deriving (Eq, Show)
