{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Admin.Types where

import Types

data AdminMock
  = SelectKeys
  | InsertReturnUser InsertUser
  | GetDay
  | GenerateTokenKey
  deriving (Eq, Show)
