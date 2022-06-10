module Spec.MakeCatResp.Types where

import Types

data MakeCatRMock
  = SelectCats CategoryId
  | SelectSubCats CategoryId
  deriving (Eq, Show)
