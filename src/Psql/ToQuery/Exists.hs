{-# LANGUAGE FlexibleInstances #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.ToQuery.Exists where

import Data.List (intercalate)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Query,In(In))
import Database.PostgreSQL.Simple.Types (PGArray(PGArray))
import Types
import Data.Time.Calendar ( Day)
import Data.Text (Text,pack, unpack,cons,snoc)
import Psql.ToQuery 
import Psql.ToQuery.Select


data Exists =
  Exists Table Where

instance ToStr Exists where
  toStr (Exists t wh) = 
    "SELECT EXISTS (SELECT 1 FROM " ++ t ++ " WHERE " ++ toStr wh ++ ")"

instance ToVal Exists where
  toVal (Exists t wh)  = toVal wh

