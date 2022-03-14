{-# LANGUAGE FlexibleInstances #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.ToQuery.Delete where

import Data.List (intercalate)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Query,In(In))
import Database.PostgreSQL.Simple.Types (PGArray(PGArray))
import Types
import Data.Time.Calendar ( Day)
import Data.Text (Text,pack, unpack,cons,snoc)
import Psql.ToQuery 
import Psql.ToQuery.Select


data Delete =
  Delete Table Where

instance ToStr Delete where
  toStr (Delete t wh) = 
    "DELETE FROM " ++ t ++ " WHERE " ++ toStr wh

instance ToVal Delete where
  toVal (Delete t wh)  = toVal wh

