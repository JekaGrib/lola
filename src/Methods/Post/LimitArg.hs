{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Post.LimitArg (LimitArg (..), FilterArg (..), SortArg (..), chooseArgs, isDateASC) where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (elemIndex, sortBy)
import Data.Text (Text, concat, pack, toUpper, unpack)
import Network.HTTP.Types.URI (queryToQueryText)
import Network.Wai (Request (..))
import Oops (ReqError (..))
import ParseQueryStr (checkLength, checkMaybeParam)
import TryRead (tryReadDay, tryReadId, tryReadIdArray)
import Types
import Database.PostgreSQL.Simple.Types (PGArray(..),In(..))



data LimitArg = LimitArg [FilterArg] [SortArg]

data FilterArg = FilterArg {tableFil :: String, whereFil :: String, valuesFil :: ([DbValue], [DbValue])}

data SortArg = SortArg {tableSort :: String, orderBySort :: String, sortDate :: SortDate}

data SortArgPriority = SortArgPriority {sortArgSAP :: SortArg, sortPrioSAP :: SortPriority}

data SortDate = DateASC | DateDESC
  deriving (Eq, Show, Read)

type SortPriority = Int

defDateSort :: SortDate
defDateSort = DateDESC

isDateASC :: [SortArg] -> Bool
isDateASC = foldr (\(SortArg _ _ c) cont -> (c == DateASC) || cont) False

chooseArgs :: (Monad m) => Request -> ExceptT ReqError m LimitArg
chooseArgs req = do
  checkReqLength req
  let filterDateList = ["created_at", "created_at_lt", "created_at_gt"]
  let filterTagList = ["tag", "tags_in", "tags_all"]
  let filterInList = ["name_in", "text_in", "everywhere_in"]
  let filterParamsList = filterDateList ++ ["category_id", "author_name"] ++ filterTagList ++ filterInList
  let sortList = ["sort_by_pics_number", "sort_by_category", "sort_by_author", "sort_by_date"]
  mapM_ (checkComb req) [filterDateList, filterTagList, filterInList]
  maybeFilterArgs <- mapM (chooseFilterArgPreCheck req) filterParamsList
  let filterArgs = concatMap toList maybeFilterArgs
  maybeSortArgPrios <- mapM (chooseSortArgPrioPreCheck req) sortList
  let sortArgs = sortArsInOrder maybeSortArgPrios
  return $ LimitArg filterArgs sortArgs

checkComb :: (Monad m) => Request -> [Text] -> ExceptT ReqError m ()
checkComb req list = case fmap (isExistParam req) list of
  (True : True : _) -> throwE $ SimpleError "Invalid combination of filter parameters"
  (_ : True : True : _) -> throwE $ SimpleError "Invalid combination of filter parameters"
  (True : _ : True : _) -> throwE $ SimpleError "Invalid combination of filter parameters"
  _ -> return ()

chooseFilterArgPreCheck :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m (Maybe FilterArg)
chooseFilterArgPreCheck req paramKey = do
  maybeParam <- checkMaybeParam req paramKey
  case maybeParam of
    Just txt -> chooseFilterArg paramKey txt
    Nothing -> return Nothing

chooseFilterArg :: (Monad m) => QueryParamKey -> Text -> ExceptT ReqError m (Maybe FilterArg)
chooseFilterArg paramKey x = let val = Txt x in case paramKey of
  "created_at" -> do
    _ <- tryReadDay paramKey x
    let table = ""
    let where' = "post_create_date = ?"
    let values = ([], [val])
    return . Just $ FilterArg table where' values
  "created_at_lt" -> do
    _ <- tryReadDay paramKey x
    let table = ""
    let where' = "post_create_date < ?"
    let values = ([], [val])
    return . Just $ FilterArg table where' values
  "created_at_gt" -> do
    _ <- tryReadDay paramKey x
    let table = ""
    let where' = "post_create_date > ?"
    let values = ([], [val])
    return . Just $ FilterArg table where' values
  "category_id" -> do
    _ <- tryReadId paramKey x
    let table = ""
    let where' = "post_category_id = ?"
    let values = ([], [val])
    return . Just $ FilterArg table where' values
  "tag" -> do
    _ <- tryReadId paramKey x
    let table = "JOIN (SELECT post_id FROM poststags WHERE tag_id = ? GROUP BY post_id) AS t ON posts.post_id=t.post_id"
    let where' = "true"
    let values = ([val], [])
    return . Just $ FilterArg table where' values
  "tags_in" -> do
    xs <- tryReadIdArray paramKey x
    let table = "JOIN (SELECT post_id FROM poststags WHERE tag_id IN ? GROUP BY post_id) AS t ON posts.post_id=t.post_id"
    let where' = "true"
    let values = ([IdIn (In xs)], [])
    return . Just $ FilterArg table where' values
  "tags_all" -> do
    xs <- tryReadIdArray paramKey x
    let table = "JOIN (SELECT post_id, array_agg(ARRAY[tag_id]::bigint[]) AS tags_id FROM poststags GROUP BY post_id) AS t ON posts.post_id=t.post_id"
    let where' = "tags_id @> ?"
    let values = ([], [IdArray (PGArray xs)])
    return . Just $ FilterArg table where' values
  "name_in" -> do
    _ <- checkLength 50 paramKey x
    let table = ""
    let where' = "post_name ILIKE ?"
    let values = ([], [Txt $ Data.Text.concat ["%", escape x, "%"]])
    return . Just $ FilterArg table where' values
  "text_in" -> do
    _ <- checkLength 50 paramKey x
    let table = ""
    let where' = "post_text ILIKE ?"
    let values = ([], [Txt $ Data.Text.concat ["%", escape x, "%"]])
    return . Just $ FilterArg table where' values
  "everywhere_in" -> do
    _ <- checkLength 50 paramKey x
    let table = "JOIN users AS usrs ON authors.user_id=usrs.user_id JOIN categories AS c ON c.category_id=posts.post_category_id JOIN (SELECT pt.post_id, bool_or(tag_name ILIKE ? ) AS isintag FROM poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id  GROUP BY pt.post_id) AS tg ON tg.post_id=posts.post_id"
    let where' = "(post_text ILIKE ? OR post_name ILIKE ? OR usrs.first_name ILIKE ? OR c.category_name ILIKE ? OR isintag = TRUE)"
    let values = ([Txt $ Data.Text.concat ["%", escape x, "%"]], replicate 4 $ Txt $ Data.Text.concat ["%", escape x, "%"])
    return . Just $ FilterArg table where' values
  "author_name" -> do
    _ <- checkLength 50 paramKey x
    let table = "JOIN users AS us ON authors.user_id=us.user_id"
    let where' = "us.first_name = ?"
    let values = ([], [val])
    return . Just $ FilterArg table where' values
  _ -> throwE $ SimpleError $ "Can`t parse query parameter" ++ unpack paramKey

chooseSortArgPrioPreCheck :: (Monad m) => Request -> Text -> ExceptT ReqError m (Maybe SortArgPriority)
chooseSortArgPrioPreCheck req paramKey = do
  maybeParam <- checkMaybeParam req paramKey
  case maybeParam of
    Just txt -> do
      maybeSortArg <- chooseSortArg paramKey txt
      return $ addSortPriority req paramKey maybeSortArg
    Nothing -> return Nothing

chooseSortArg :: (Monad m) => Text -> Text -> ExceptT ReqError m (Maybe SortArg)
chooseSortArg paramKey "DESC" = case paramKey of
  "sort_by_pics_number" -> do
    let joinTable = "JOIN (SELECT post_id, count (post_id) AS count_pics FROM postspics GROUP BY post_id) AS counts ON posts.post_id=counts.post_id"
    let orderBy = "count_pics DESC"
    return . Just $ SortArg joinTable orderBy defDateSort
  "sort_by_category" -> do
    let joinTable = "JOIN categories ON posts.post_category_id=categories.category_id"
    let orderBy = "category_name DESC"
    return . Just $ SortArg joinTable orderBy defDateSort
  "sort_by_author" -> do
    let joinTable = "JOIN users AS u ON authors.user_id=u.user_id"
    let orderBy = "u.first_name DESC"
    return . Just $ SortArg joinTable orderBy defDateSort
  "sort_by_date" -> do
    let joinTable = ""
    let orderBy = "post_create_date DESC"
    return . Just $ SortArg joinTable orderBy DateDESC
  _ -> throwE $ SimpleError $ "Can`t parse query parameter: " ++ unpack paramKey
chooseSortArg paramKey "ASC" = case paramKey of
  "sort_by_pics_number" -> do
    let joinTable = "JOIN (SELECT post_id, count (post_id) AS count_pics FROM postspics GROUP BY post_id) AS counts ON posts.post_id=counts.post_id"
    let orderBy = "count_pics ASC"
    return . Just $ SortArg joinTable orderBy defDateSort
  "sort_by_category" -> do
    let joinTable = "JOIN categories ON posts.post_category_id=categories.category_id"
    let orderBy = "category_name ASC"
    return . Just $ SortArg joinTable orderBy defDateSort
  "sort_by_author" -> do
    let joinTable = "JOIN users AS u ON authors.user_id=u.user_id"
    let orderBy = "u.first_name ASC"
    return . Just $ SortArg joinTable orderBy defDateSort
  "sort_by_date" -> do
    let joinTable = ""
    let orderBy = "post_create_date ASC"
    return . Just $ SortArg joinTable orderBy DateASC
  _ -> throwE $ SimpleError $ "Can`t parse query parameter: " ++ unpack paramKey
chooseSortArg paramKey txt
  | Data.Text.toUpper txt == "ASC" = chooseSortArg paramKey "ASC"
  | Data.Text.toUpper txt == "DESC" = chooseSortArg paramKey "DESC"
  | otherwise = throwE $ SimpleError $ "Invalid sort value: " ++ unpack txt ++ ". It should be only 'ASC' or 'DESC'"

checkReqLength :: (Monad m) => Request -> ExceptT ReqError m ()
checkReqLength req = case splitAt 20 $ queryString req of
  (_, []) -> return ()
  _ -> throwE $ SimpleError "There is should be less then 20 query string parameters"

sortArsInOrder :: [Maybe SortArgPriority] -> [SortArg]
sortArsInOrder = map sortArgSAP . sortBy (compare `on` sortPrioSAP) . concatMap toList

addSortPriority :: Request -> QueryParamKey -> Maybe SortArg -> Maybe SortArgPriority
addSortPriority req paramKey maybeSortArg = do
  sortArg <- maybeSortArg
  num <- findParamIndex req paramKey
  return $ SortArgPriority sortArg num

findParam :: Request -> Text -> Maybe (Maybe Text)
findParam req txt = lookup txt . queryToQueryText $ queryString req

findParamIndex :: Request -> QueryParamKey -> Maybe Int
findParamIndex req paramKey = elemIndex paramKey . fmap fst . queryToQueryText $ queryString req

isExistParam :: Request -> Text -> Bool
isExistParam req txt = case findParam req txt of
  Just _ -> True
  Nothing -> False

escape :: Text -> Text
escape xs = pack $ concatMap escapeChar (unpack xs)

escapeChar :: Char -> String
escapeChar '\\' = "\\\\"
escapeChar '%' = "\\%"
escapeChar '_' = "\\_"
escapeChar a = [a]
