{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Common.ToQuery (toSelQ, toSelLimQ, toUpdQ, toDelQ, toExQ, toInsRetQ, toInsManyQ) where

import Data.List (intercalate)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Query)
import Types

toSelQ :: Table -> [Param] -> Where -> Query
toSelQ table params where' =
  fromString $ "SELECT " ++ intercalate ", " params ++ " FROM " ++ table ++ " WHERE " ++ where'

toSelLimQ :: Table -> OrderBy -> Page -> Limit -> [Param] -> Where -> Query
toSelLimQ table orderBy page limitNumber params where' =
  fromString $ "SELECT " ++ intercalate ", " params ++ " FROM " ++ table ++ " WHERE " ++ where' ++ " ORDER BY " ++ orderBy ++ " OFFSET " ++ show ((page -1) * limitNumber) ++ " LIMIT " ++ show (page * limitNumber)

toUpdQ :: Table -> Set -> Where -> Query
toUpdQ table set where' =
  fromString $ "UPDATE " ++ table ++ " SET " ++ set ++ " WHERE " ++ where'

toDelQ :: Table -> Where -> Query
toDelQ table where' =
  fromString $ "DELETE FROM " ++ table ++ " WHERE " ++ where'

toExQ :: Table -> Where -> Query
toExQ table where' =
  fromString $ "SELECT EXISTS (SELECT 1 FROM " ++ table ++ " WHERE " ++ where' ++ ")"

toInsRetQ :: Table -> ReturnParam -> [Param] -> Query
toInsRetQ table returnName insNames =
  fromString $ "INSERT INTO " ++ table ++ " ( " ++ intercalate "," insNames ++ " ) VALUES ( " ++ (intercalate "," . fmap (const "?") $ insNames) ++ " ) RETURNING " ++ returnName

toInsManyQ :: Table -> [Param] -> Query
toInsManyQ table insNames =
  fromString $ "INSERT INTO " ++ table ++ " ( " ++ intercalate "," insNames ++ " ) VALUES ( " ++ (intercalate "," . fmap (const "?") $ insNames) ++ " ) "

class ToStr a where
  toStr :: a -> String

data Select =
  Select [DbKey] Table Where

instance ToStr Select where
  toStr (Select keys t wh) = 
    "SELECT " ++ intercalate ", " keys ++ " FROM " ++ table ++ " WHERE " ++ toStr wh

data Where =
  Where Predicate
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
  toStr WhereOr xs = "(" ++ intercalate " OR " xs ++ ")"
  toStr WhereAnd xs = "(" ++ intercalate " AND " xs ++ ")"

data Update = 
  Update Table Set Where

instance ToStr Update where
  toStr (Update t set wh) = 
    "UPDATE " ++ t ++ " SET " ++ toStr set ++ " WHERE " ++ toStr wh

data Set =
  SetPair Predicate DbValue

instance ToStr Set where
  toStr (SetPair str _) = str

data Delete =
  Delete Table Where

instance ToStr Delete where
  toStr (Delete t wh) = 
    "DELETE FROM " ++ t ++ " WHERE " ++ toStr wh

data Exists =
  Exists Table Where

instance ToStr Exists where
  toStr (Exists t wh) = 
    "SELECT EXISTS (SELECT 1 FROM " ++ t ++ " WHERE " ++ toStr wh ++ ")"

data InsertRet =
  InsertRet Table DbReturnKey [InsertPair]

instance ToStr InsertRet where
  toStr (InsertRet t retKey insPairs) = 
    "INSERT INTO " ++ t ++ toStr insPairs ++ " RETURNING " ++ returnName

data InsertPair = 
  InsertPair {insKey :: DbKey, insVal :: DbValue}

instance ToStr InsertPair where
  toStr (InsertPair k _) = " ( " ++ k ++ " ) VALUES ( ? )"

instance ToStr [InsertPair] where
  toStr insPairs = 
    " ( " ++ intercalate "," (map insKey insPairs)  ++ " ) VALUES ( " ++ (intercalate "," . fmap (const "?") $ insPairs) ++ " )"

data InsertMany =
  InsertMany Table [InsertManyPair]

instance ToStr InsertMany where
  toStr (InsertMany t insPairs) = 
    "INSERT INTO " ++ t ++ toStr insPairs

data InsertManyPair = 
  InsertPair {insManyKey :: DbKey, insManyVal :: DbValue}

instance ToStr InsertManyPair where
  toStr (InsertManyPair k _) = " ( " ++ k ++ " ) VALUES ( ? )"

instance ToStr [InsertManyPair] where
  toStr insPairs = 
    " ( " ++ intercalate "," (map insManyKey insPairs)  ++ " ) VALUES ( " ++ intercalate "," ( fmap (const "?") insPairs) ++ " )"

data SortOrd = ASC | DESC
  deriving (Eq,Show)

class AddJoinTable a where
  addJoinTable :: a -> JoinTable

instance AddJoinTable [a] where
  addJoinTable xs = concatMap addJoinTable xs

data OrderBy =
  ByPostPicsNumb SortOrd
  | ByPostCat SortOrd
  | ByPostAuthor SortOrd
  | ByPostDate SortOrd
  | ByPostId SortOrd
  | ByCommId SortOrd
  | ByDraftId SortOrd
  | OrderList [OrderBy]

instance ToStr OrderBy where
  toStr (ByPostPicsNumb sOrd) = "count_pics " ++ show sOrd
  toStr (ByPostCat sOrd)      = "category_name " ++ show sOrd
  toStr (ByPostAuthor sOrd)   = "u.first_name " ++ show sOrd
  toStr (ByPostDate sOrd)     = "post_create_date " ++ show sOrd
  toStr (ByPostId sOrd)       = "posts.post_id " ++ show sOrd
  toStr (ByCommId sOrd)       = "comment_id " ++ show sOrd
  toStr (ByDraftId sOrd)      = "draft_id " ++ show sOrd

instance AddJoinTable OrderBy where
  addJoinTable (ByPostPicsNumb _) =  
    " JOIN (SELECT post_id, count (post_id) AS count_pics FROM postspics GROUP BY post_id) AS counts ON posts.post_id=counts.post_id"
  addJoinTable (ByPostCat   _)    =
    " JOIN categories ON posts.post_category_id=categories.category_id"  
  addJoinTable (ByPostAuthor _)   =  
    " JOIN users AS u ON authors.user_id=u.user_id"
  addJoinTable _                  =  ""
  
