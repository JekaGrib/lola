{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Psql.ToQuery where

import Data.String (fromString)
import Database.PostgreSQL.Simple (Query)
import Types

toQ :: (ToStr a) => a -> Query
toQ = fromString . toStr

class ToStr a where
  toStr :: a -> String

class ToVal a where
  toVal :: a -> [DbValue]

class AddJoinTable a where
  addJoinTable :: a -> JoinTable

instance (AddJoinTable a) => AddJoinTable [a] where
  addJoinTable = concatMap addJoinTable
