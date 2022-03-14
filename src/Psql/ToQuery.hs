{-# LANGUAGE FlexibleInstances #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.ToQuery where

import Data.List (intercalate)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Query,In(In))
import Database.PostgreSQL.Simple.Types (PGArray(PGArray))
import Types
import Data.Time.Calendar ( Day)
import Data.Text (Text,pack, unpack,cons,snoc)


toQ :: (ToStr a) => a -> Query
toQ = fromString . toStr


class ToStr a where
  toStr :: a -> String

class ToVal a where
  toVal :: a -> [DbValue]

class AddJoinTable a where
  addJoinTable :: a -> JoinTable


instance (AddJoinTable a) => AddJoinTable [a] where
  addJoinTable xs = concatMap addJoinTable xs


