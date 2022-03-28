{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.MakeCatResp.Types where

import Types

data MakeCatRMock
  = SelectCats CategoryId
  | SelectSubCats CategoryId
  deriving (Eq, Show)
