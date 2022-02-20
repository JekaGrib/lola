{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Types where

import Logger

data MockAction = 
  LOG Priority 
  | EXISTCHEK Table  Where  DbParamValue 
  | INSERTDATA Table  DbReturnParamKey  [DbInsertParamKey]  [DbParamValue]
  | SELECTDATA Table  [DbSelectParamKey]  Where  [DbParamValue]
  | UPDATEDATA Table  ToUpdate  Where  [DbParamValue]
  | DELETEDATA Table  Where  [DbParamValue]
  | INSERTMANYDATA Table  [DbInsertParamKey]  [(DbNumValue, DbNumValue)]
  | SELECTLIMITDATA Table  OrderBy  Page  Limit  [DbSelectParamKey]  Where  [DbParamValue]  [FilterArg] [SortArg]
  | TRANSACTIONOPEN 
  | TRANSACTIONCLOSE
  | TRANSACTIONunROLL
  deriving (Eq, Show)
