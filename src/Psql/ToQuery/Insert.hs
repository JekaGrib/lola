{-# LANGUAGE FlexibleInstances #-}

module Psql.ToQuery.Insert where

import Data.List (intercalate)
import Psql.ToQuery (ToStr (..), ToVal (..))
import Types

data InsertRet
  = InsertRet Table [InsertPair] DbReturnKey

instance ToStr InsertRet where
  toStr (InsertRet table insertPairs retKey) =
    "INSERT INTO " ++ table ++ toStr insertPairs ++ " RETURNING " ++ retKey

instance ToVal InsertRet where
  toVal (InsertRet _ insertPairs _) = toVal insertPairs

data InsertPair = InsertPair {insKey :: DbKey, insVal :: DbValue}

instance ToStr InsertPair where
  toStr (InsertPair k _) = " ( " ++ k ++ " ) VALUES ( ? )"

instance ToVal InsertPair where
  toVal (InsertPair _ val) = [val]

instance ToStr [InsertPair] where
  toStr insertPairs =
    " ( " ++ intercalate "," (map insKey insertPairs) ++ " ) VALUES ( " ++ (intercalate "," . fmap (const "?") $ insertPairs) ++ " )"

instance ToVal [InsertPair] where
  toVal = concatMap toVal

data InsertMany
  = InsertMany Table InsertManyPair

instance ToStr InsertMany where
  toStr (InsertMany table insertPair) =
    "INSERT INTO " ++ table ++ toStr insertPair

data InsertManyPair = InsertManyPair {insManyKey :: (DbKey, DbKey), insManyVal :: [(Id, Id)]}

instance ToStr InsertManyPair where
  toStr (InsertManyPair (key, val) _) = " (" ++ key ++ "," ++ val ++ ") VALUES (?,?)"
