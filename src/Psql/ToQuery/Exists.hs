{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Psql.ToQuery.Exists where

import Types
import Psql.ToQuery (ToVal(..),ToStr(..))
import Psql.ToQuery.Select (Where)


data Exists =
  Exists Table Where

instance ToStr Exists where
  toStr (Exists t wh) = 
    "SELECT EXISTS (SELECT 1 FROM " ++ t ++ " WHERE " ++ toStr wh ++ ")"

instance ToVal Exists where
  toVal (Exists _ wh)  = toVal wh

