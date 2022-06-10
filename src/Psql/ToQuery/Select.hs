module Psql.ToQuery.Select where

import Data.List (intercalate)
import Psql.ToQuery (ToStr (..), ToVal (..))
import Types

data Select
  = Select [DbKey] Table Where

instance ToStr Select where
  toStr (Select keys t wh) =
    "SELECT " ++ intercalate ", " keys ++ " FROM " ++ t ++ " WHERE " ++ toStr wh

instance ToVal Select where
  toVal (Select _ _ wh) = toVal wh

class ToWhere a where
  toWhere :: a -> Where

data Where
  = Where Predicate
  | WherePair Predicate DbValue
  | WhereSelect Predicate Select
  | WhereSelectPair Select Predicate DbValue
  | WhereOr [Where]
  | WhereAnd [Where]

instance ToStr Where where
  toStr (Where str) = str
  toStr (WherePair str _) = str
  toStr (WhereSelect str sel) = str ++ "(" ++ toStr sel ++ ")"
  toStr (WhereSelectPair sel str _) = "(" ++ toStr sel ++ ")" ++ str
  toStr (WhereOr xs) = "(" ++ intercalate " OR " (map toStr xs) ++ ")"
  toStr (WhereAnd xs) = "(" ++ intercalate " AND " (map toStr xs) ++ ")"

instance ToVal Where where
  toVal (Where _) = []
  toVal (WherePair _ val) = [val]
  toVal (WhereSelect _ sel) = toVal sel
  toVal (WhereSelectPair sel _ val) = toVal sel ++ [val]
  toVal (WhereOr xs) = concatMap toVal xs
  toVal (WhereAnd xs) = concatMap toVal xs
