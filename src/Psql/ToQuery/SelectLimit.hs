{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Psql.ToQuery.SelectLimit where

import Data.List (intercalate)
import Data.Text (Text, cons, pack, snoc, unpack)
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple (In (In))
import Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import Psql.ToQuery (AddJoinTable (..), ToStr (..), ToVal (..))
import Psql.ToQuery.Select (Select (..), ToWhere (..), Where (..))
import Types

data SelectLim
  = SelectLim [DbKey] Table Where [Filter] OrderBy Page Limit

instance ToStr SelectLim where
  toStr (SelectLim keys t wh filterArgs ord _ _) =
    let table = t ++ addJoinTable ord ++ addJoinTable filterArgs
     in "SELECT " ++ intercalate ", " keys ++ " FROM " ++ table
          ++ " WHERE "
          ++ toStr wh
          ++ " ORDER BY "
          ++ toStr ord
          ++ " OFFSET ? LIMIT ?"

instance ToVal SelectLim where
  toVal (SelectLim _ _ wh _ _ page limit) =
    toVal wh ++ [Num ((page - 1) * limit), Num (page * limit)]

data OrderBy
  = ByPostPicsNumb SortOrd
  | ByPostCat SortOrd
  | ByPostAuthor SortOrd
  | ByPostDate SortOrd
  | ByPostId SortOrd
  | ByCommId SortOrd
  | ByDraftId SortOrd
  | OrderList [OrderBy]
  deriving (Eq, Show)

instance ToStr OrderBy where
  toStr (ByPostPicsNumb sOrd) = "count_pics " ++ show sOrd
  toStr (ByPostCat sOrd) = "category_name " ++ show sOrd
  toStr (ByPostAuthor sOrd) = "u.first_name " ++ show sOrd
  toStr (ByPostDate sOrd) = "post_create_date " ++ show sOrd
  toStr (ByPostId sOrd) = "posts.post_id " ++ show sOrd
  toStr (ByCommId sOrd) = "comment_id " ++ show sOrd
  toStr (ByDraftId sOrd) = "draft_id " ++ show sOrd
  toStr (OrderList xs) = intercalate "," . map toStr $ xs

instance AddJoinTable OrderBy where
  addJoinTable (ByPostPicsNumb _) =
    " JOIN (SELECT post_id, count (post_id) AS count_pics FROM postspics GROUP BY post_id) AS counts ON posts.post_id=counts.post_id"
  addJoinTable (ByPostCat _) =
    " JOIN categories ON posts.post_category_id=categories.category_id"
  addJoinTable (ByPostAuthor _) =
    " JOIN users AS u ON authors.user_id=u.user_id"
  addJoinTable (OrderList xs) = addJoinTable xs
  addJoinTable _ = ""

data Filter
  = CreatedF CreatedF
  | CatIdF CategoryId
  | TagF TagF
  | InF InF
  | AuthorNameF Text
  deriving (Eq, Show)

data CreatedF
  = At Day
  | AtLt Day
  | AtGt Day
  deriving (Eq, Show)

data TagF
  = TagIdF TagId
  | TagsIn [TagId]
  | TagsAll [TagId]
  deriving (Eq, Show)

data InF
  = PostText Text
  | Name Text
  | UsersName Text
  | CatName Text
  | TagName Text
  | EveryWhere [InF]
  deriving (Eq, Show)

instance ToWhere Filter where
  toWhere (CatIdF catId) = WherePair " post_category_id = ? " (Id catId)
  toWhere (AuthorNameF auName) = WherePair " usrs.first_name = ? " (Txt auName)
  toWhere (InF f) = toWhere f
  toWhere (CreatedF f) = toWhere f
  toWhere (TagF f) = toWhere f

instance AddJoinTable Filter where
  addJoinTable (AuthorNameF _) = " JOIN users AS usrs ON authors.user_id=usrs.user_id"
  addJoinTable (InF f) = addJoinTable f
  addJoinTable _ = ""

toILike :: Text -> Text
toILike = cons '%' . flip snoc '%'

escape :: Text -> Text
escape = pack . concatMap escapeChar . unpack

escapeChar :: Char -> String
escapeChar '\\' = "\\\\"
escapeChar '%' = "\\%"
escapeChar '_' = "\\_"
escapeChar a = [a]

instance ToWhere InF where
  toWhere (PostText txt) =
    WherePair " post_text ILIKE ? " (Txt (toILike txt))
  toWhere (Name txt) =
    WherePair " post_name ILIKE ? " (Txt (toILike txt))
  toWhere (UsersName txt) =
    WherePair " us.first_name ILIKE ? " (Txt (toILike txt))
  toWhere (TagName txt) =
    let sel = Select ["post_id"] "tags JOIN poststags AS pt ON tags.tag_id = pt.tag_id" (WherePair " tag_name ILIKE ? " (Txt (toILike txt)))
     in WhereSelect " posts.post_id IN " sel
  toWhere (CatName txt) =
    WherePair " c.category_name ILIKE ? " (Txt (toILike txt))
  toWhere (EveryWhere xs) = WhereOr (fmap toWhere xs)

instance AddJoinTable InF where
  addJoinTable (UsersName _) = " JOIN users AS us ON authors.user_id=us.user_id "
  addJoinTable (CatName _) = " JOIN categories AS c ON c.category_id=posts.post_category_id "
  addJoinTable (EveryWhere xs) = addJoinTable xs
  addJoinTable _ = ""

instance ToWhere CreatedF where
  toWhere (At day) =
    WherePair " post_create_date = ? " (Day day)
  toWhere (AtLt day) =
    WherePair " post_create_date < ? " (Day day)
  toWhere (AtGt day) =
    WherePair " post_create_date > ? " (Day day)

instance ToWhere TagF where
  toWhere (TagIdF iD) =
    let sel = Select ["post_id"] "poststags" $ WherePair " tag_id = ? " (Id iD)
     in WhereSelect " posts.post_id IN " sel
  toWhere (TagsIn idS) =
    let sel = Select ["post_id"] "poststags" $ WherePair " tag_id IN ? " (IdIn (In idS))
     in WhereSelect " posts.post_id IN " sel
  toWhere (TagsAll idS) =
    let sel = Select ["array_agg(tag_id)"] "poststags" $ Where "posts.post_id = poststags.post_id"
     in WhereSelectPair sel " @>?::bigint[] " (IdArray (PGArray idS))
