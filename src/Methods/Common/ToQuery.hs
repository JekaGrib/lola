{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Common.ToQuery (toSelQ, toSelLimQ, toUpdQ, toDelQ, toExQ, toInsRetQ, toInsManyQ) where

import Data.List (intercalate)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Query)
import Types

toQ :: (ToStr a) => a -> Query
toQ = fromString . toStr


class ToStr a where
  toStr :: a -> String

class ToVal a where
  toVal :: a -> [DbValue]

data Select =
  Select [DbKey] Table Where

instance ToStr Select where
  toStr (Select keys t wh) = 
    "SELECT " ++ intercalate ", " keys ++ " FROM " ++ table ++ " WHERE " ++ toStr wh

instance ToVal Select where
  toVal (Select keys t wh) = toVal wh

data SelectLim =
  SelectLim [DbKey] Table Where OrderBy Page Limit

instance ToStr SelectLim where
  toStr (SelectLim keys t wh ord page limit) = 
    "SELECT " ++ intercalate ", " keys ++ " FROM " ++ t 
    ++ " WHERE " ++ toStr wh ++ " ORDER BY " ++ toStr ord 
    ++ " OFFSET ? LIMIT ?" 

instance ToVal SelectLim where
  toVal (SelectLim keys t wh ord page limit) = 
    toVal wh ++ toVal ord 
    ++ [Num ((page - 1) * limitNumber),Num (page * limitNumber)]

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

instance ToVal Where where
  toVal (Where _) = []
  toVal (WherePair _ val) = [val]
  toVal (WhereSelect _ sel) = toVal sel 
  toVal (WhereSelectPair sel _ val) =  toVal sel  ++ [val]
  toVal WhereOr xs = map toVal xs
  toVal WhereAnd xs = map toVal xs

data Update = 
  Update Table [Set] Where

instance ToStr Update where
  toStr (Update t sets wh) = 
    "UPDATE " ++ t ++ " SET " ++ toStr sets ++ " WHERE " ++ toStr wh

instance ToVal Select where
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

data Delete =
  Delete Table Where

instance ToStr Delete where
  toStr (Delete t wh) = 
    "DELETE FROM " ++ t ++ " WHERE " ++ toStr wh

instance ToVal Delete where
  toVal (Delete t wh)  = toVal wh

data Exists =
  Exists Table Where

instance ToStr Exists where
  toStr (Exists t wh) = 
    "SELECT EXISTS (SELECT 1 FROM " ++ t ++ " WHERE " ++ toStr wh ++ ")"

instance ToVal Exists where
  toVal (Exists t wh)  = toVal wh

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
  InsertPair {insManyKey :: (DbKey,DbKey) , insManyVal :: [(Id,Id)]}

instance ToStr InsertManyPair where
  toStr (InsertManyPair (k1,k2) _) = " (" ++ k1 ++ "," ++ k2 ++ ") VALUES (?,?)"



data SortOrd = ASC | DESC
  deriving (Eq,Show)

class AddJoinTable a where
  addJoinTable :: a -> JoinTable

instance (AddJoinTable a) => AddJoinTable [a] where
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
  toStr (OrderList xs)        = intercalate "," . map toStr $ xs 

instance AddJoinTable OrderBy where
  addJoinTable (ByPostPicsNumb _) =  
    " JOIN (SELECT post_id, count (post_id) AS count_pics FROM postspics GROUP BY post_id) AS counts ON posts.post_id=counts.post_id"
  addJoinTable (ByPostCat   _)    =
    " JOIN categories ON posts.post_category_id=categories.category_id"  
  addJoinTable (ByPostAuthor _)   =  
    " JOIN users AS u ON authors.user_id=u.user_id"
  addJoinTable _                  =  ""
  
