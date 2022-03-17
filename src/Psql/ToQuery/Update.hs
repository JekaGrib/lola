{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Psql.ToQuery.Update where

import Data.List (intercalate)
import Types
import Psql.ToQuery (ToVal(..),ToStr(..))
import Psql.ToQuery.Select (Where(..))



data Update = 
  Update Table [Set] Where

instance ToStr Update where
  toStr (Update t sets wh) = 
    "UPDATE " ++ t ++ " SET " ++ toStr sets ++ " WHERE " ++ toStr wh

instance ToVal Update where
  toVal (Update _ set wh) = toVal set ++ toVal wh

data Set =
  SetPair Predicate DbValue

instance ToStr Set where
  toStr (SetPair str _) = str

instance ToStr [Set] where
  toStr sets = intercalate "," $ fmap toStr sets

instance ToVal Set where
  toVal (SetPair _ val) = [val]

instance ToVal [Set] where
  toVal = concatMap toVal 

