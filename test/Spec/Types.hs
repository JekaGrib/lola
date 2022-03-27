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

{-data MockAction =
  LOG Priority
  | EXISTCHEK UncheckedExId
  | INSERTDATA Table  DbReturnParamKey  [DbInsertParamKey]  [DbParamValue]
  | SELECTDATA Table  [DbSelectParamKey]  Where  [DbParamValue]
  | UPDATEDATA Table  ToUpdate  Where  [DbParamValue]
  | DELETEDATA Table  Where  [DbParamValue]
  | INSERTMANYDATA Table  [DbInsertParamKey]  [(DbNumValue, DbNumValue)]
  | SELECTLIMITDATA Table  OrderBy  Page  Limit  [DbSelectParamKey]  Where  [DbParamValue]  [FilterArg] [SortArg]
  | TRANSACTIONOPEN
  | TRANSACTIONCLOSE
  | TRANSACTIONunROLL
  deriving (Eq, Show)-}
