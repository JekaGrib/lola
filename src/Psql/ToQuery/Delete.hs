{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Psql.ToQuery.Delete where

import Psql.ToQuery (ToStr (..), ToVal (..))
import Psql.ToQuery.Select (Where)
import Types

data Delete
  = Delete Table Where

instance ToStr Delete where
  toStr (Delete t wh) =
    "DELETE FROM " ++ t ++ " WHERE " ++ toStr wh

instance ToVal Delete where
  toVal (Delete _ wh) = toVal wh
