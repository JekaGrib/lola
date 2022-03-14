{-# LANGUAGE FlexibleInstances #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.ToQuery.Update where

import Data.List (intercalate)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Query,In(In))
import Database.PostgreSQL.Simple.Types (PGArray(PGArray))
import Types
import Data.Time.Calendar ( Day)
import Data.Text (Text,pack, unpack,cons,snoc)
import Psql.ToQuery 
import Psql.ToQuery.Select



data Update = 
  Update Table [Set] Where

instance ToStr Update where
  toStr (Update t sets wh) = 
    "UPDATE " ++ t ++ " SET " ++ toStr sets ++ " WHERE " ++ toStr wh

instance ToVal Update where
  toVal (Update t set wh) = toVal set ++ toVal wh

data Set =
  SetPair Predicate DbValue

instance ToStr Set where
  toStr (SetPair str _) = str

instance ToStr [Set] where
  toStr sets = intercalate "," $ fmap toStr sets

instance ToVal Set where
  toVal (SetPair _ val) = [val]

instance ToVal [Set] where
  toVal sets = concatMap toVal sets  

