{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}

module TypesTest where

import           Logger




data MockAction = EXISTCHEK | LOG Priority | INSERTDATA | SELECTDATA | UPDATEDATA | DELETEDATA | INSERTMANYDATA | SELECTLIMITDATA
  deriving (Eq,Show)

