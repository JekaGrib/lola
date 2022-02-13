{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module TypesTest where

import           Logger




data MockAction = EXISTCHEK | LOG Priority | INSERTDATA | SELECTDATA | UPDATEDATA | DELETEDATA | INSERTMANYDATA | SELECTLIMITDATA | TRANSACTIONOPEN |TRANSACTIONCLOSE
  deriving (Eq,Show)

