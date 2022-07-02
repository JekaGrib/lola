module Psql.ToQuery.Exists where

import Psql.ToQuery (ToStr (..), ToVal (..))
import Psql.ToQuery.Select (Where)
import Types

data Exists
  = Exists Table Where

instance ToStr Exists where
  toStr (Exists table where') =
    "SELECT EXISTS (SELECT 1 FROM " ++ table
      ++ " WHERE "
      ++ toStr where'
      ++ ")"

instance ToVal Exists where
  toVal (Exists _ where') = toVal where'
