{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Types where

import Logger

data MockAction = 
  LOG Priority 
  | EXISTCHEK 
  | INSERTDATA 
  | SELECTDATA 
  | UPDATEDATA 
  | DELETEDATA 
  | INSERTMANYDATA 
  | SELECTLIMITDATA 
  | TRANSACTIONOPEN 
  | TRANSACTIONCLOSE
  deriving (Eq, Show)
