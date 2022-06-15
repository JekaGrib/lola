{-# LANGUAGE FlexibleInstances #-}

module Psql.ToQuery.Update where

import Data.List (intercalate)
import Psql.ToQuery (ToStr (..), ToVal (..))
import Psql.ToQuery.Select (Where (..))
import Types

data Update
  = Update Table [Set] Where

instance ToStr Update where
  toStr (Update table sets where') =
    "UPDATE " ++ table ++ " SET " ++ toStr sets ++ " WHERE " ++ toStr where'

instance ToVal Update where
  toVal (Update _ set where') = toVal set ++ toVal where'

data Set
  = SetPair Predicate DbValue

instance ToStr Set where
  toStr (SetPair str _) = str

instance ToStr [Set] where
  toStr sets = intercalate "," $ fmap toStr sets

instance ToVal Set where
  toVal (SetPair _ val) = [val]

instance ToVal [Set] where
  toVal = concatMap toVal
