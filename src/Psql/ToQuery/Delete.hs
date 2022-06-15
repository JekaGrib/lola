module Psql.ToQuery.Delete where

import Psql.ToQuery (ToStr (..), ToVal (..))
import Psql.ToQuery.Select (Where)
import Types

data Delete
  = Delete Table Where

instance ToStr Delete where
  toStr (Delete table where') =
    "DELETE FROM " ++ table ++ " WHERE " ++ toStr where'

instance ToVal Delete where
  toVal (Delete _ where') = toVal where'
