{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Types where

import Logger
import Spec.Auth.Types
import Spec.Exist.Types (ExistMock)
import Spec.Tag.Types (TagMock)
import Spec.Picture.Types (PicMock)
import Spec.User.Types (UserMock)
import Spec.DeleteMany.Types (DeleteManyMock)

data MockAction
  = LOG Priority
  | TRANSACTIONOPEN
  | TRANSACTIONCLOSE
  | TRANSACTIONunROLL
  | AuthMock AuthMock
  | DeleteManyMock DeleteManyMock
  | ExistMock ExistMock
  | PicMock PicMock
  | TagMock TagMock
  | UserMock UserMock
  deriving (Eq, Show)


