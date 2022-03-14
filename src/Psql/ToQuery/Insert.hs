{-# LANGUAGE FlexibleInstances #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.ToQuery.Insert where

import Data.List (intercalate)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Query,In(In))
import Database.PostgreSQL.Simple.Types (PGArray(PGArray))
import Types
import Data.Time.Calendar ( Day)
import Data.Text (Text,pack, unpack,cons,snoc)
import Psql.ToQuery 
import Psql.ToQuery.Select



data InsertRet =
  InsertRet Table [InsertPair] DbReturnKey

instance ToStr InsertRet where
  toStr (InsertRet t insPairs retKey ) = 
    "INSERT INTO " ++ t ++ toStr insPairs ++ " RETURNING " ++ retKey

instance ToVal InsertRet where
  toVal (InsertRet t insPairs retKey)  = toVal insPairs

data InsertPair = 
  InsertPair {insKey :: DbKey, insVal :: DbValue}

instance ToStr InsertPair where
  toStr (InsertPair k _) = " ( " ++ k ++ " ) VALUES ( ? )"

instance ToVal InsertPair where
  toVal (InsertPair _ val) = [val]

instance ToStr [InsertPair] where
  toStr insPairs = 
    " ( " ++ intercalate "," (map insKey insPairs)  ++ " ) VALUES ( " ++ (intercalate "," . fmap (const "?") $ insPairs) ++ " )"

instance ToVal [InsertPair] where
  toVal insPairs = concatMap toVal insPairs


data InsertMany =
  InsertMany Table InsertManyPair

instance ToStr InsertMany where
  toStr (InsertMany t insPair) = 
    "INSERT INTO " ++ t ++ toStr insPair


data InsertManyPair = 
  InsertManyPair {insManyKey :: (DbKey,DbKey) , insManyVal :: [(Id,Id)]}

instance ToStr InsertManyPair where
  toStr (InsertManyPair (k1,k2) _) = " (" ++ k1 ++ "," ++ k2 ++ ") VALUES (?,?)"




