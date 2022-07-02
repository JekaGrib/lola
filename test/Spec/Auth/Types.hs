module Spec.Auth.Types where

import Types

newtype AuthMock = SelectTokenKeyForUser UserId
  deriving (Eq, Show)
