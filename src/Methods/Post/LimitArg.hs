{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module Methods.Post.LimitArg (LimitArg(..), FilterArg(..), SortArg(..), chooseArgs, isDateASC) where
          

import TryRead (tryReadId,tryReadDay,tryReadIdArray)
import           Data.Text                      ( pack, unpack, Text, concat, toUpper )
import Oops (ReqError(..))
import           Control.Monad.Trans.Except (ExceptT,throwE)
import           Network.Wai (Request(..))
import           Network.HTTP.Types.URI         ( queryToQueryText )
import ParseQueryStr (checkLength)
import Data.Foldable (toList)



data LimitArg = LimitArg [FilterArg] [SortArg]

data FilterArg = FilterArg {tableFil  :: String, whereFil    :: String, valuesFil :: ([Text],[Text])}
data SortArg   = SortArg   {tableSort :: String, orderBySort :: String, sortDate  :: SortDate}

data SortDate = DateASC | DateDESC 
 deriving (Eq,Show,Read)

defDateSort :: SortDate
defDateSort = DateDESC 

isDateASC :: [SortArg] -> Bool
isDateASC = foldr (\(SortArg _ _ c) cont -> (c == DateASC) || cont) False 

chooseArgs :: (Monad m) => Request -> ExceptT ReqError m LimitArg
chooseArgs req = do
  let filterDateList   = ["created_at","created_at_lt","created_at_gt"] 
  let filterTagList    = ["tag","tags_in","tags_all"] 
  let filterInList     = ["name_in","text_in","everywhere_in"] 
  let filterParamsList = filterDateList ++ ["category_id","author_name"] ++ filterTagList ++ filterInList 
  let sortList         = ["sort_by_pics_number","sort_by_category","sort_by_author","sort_by_date"] 
  mapM_ (checkComb req) [filterDateList,filterTagList,filterInList]
  maybeFilterArgs <- mapM (chooseFilterArgsPreCheck req) filterParamsList
  let filterArgs = concatMap toList maybeFilterArgs
  maybeSortArgs <- mapM (chooseSortArgsPreCheck req) sortList
  let sortArgs = concatMap toList maybeSortArgs
  return $ LimitArg filterArgs sortArgs

checkComb :: (Monad m) => Request -> [Text] -> ExceptT ReqError m ()
checkComb req list = case fmap (isExistParam req) list of
     (True:True:_)   -> throwE $ SimpleError "Invalid combination of filter parameters" 
     (_:True:True:_) -> throwE $ SimpleError "Invalid combination of filter parameters"
     (True:_:True:_) -> throwE $ SimpleError "Invalid combination of filter parameters"
     _               -> return ()

chooseFilterArgsPreCheck :: (Monad m) => Request -> Text -> ExceptT ReqError m (Maybe FilterArg)
chooseFilterArgsPreCheck req param = case findParam req param of
    Nothing -> return Nothing
    Just (Just txt) -> chooseFilterArgs txt param
    Just _ ->  throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack param

chooseFilterArgs :: (Monad m) => Text -> Text -> ExceptT ReqError m (Maybe FilterArg)
chooseFilterArgs x paramKey = case paramKey of
  "created_at" -> do
    _ <- tryReadDay paramKey x
    let table   = ""
    let where'  = "post_create_date = ?"
    let values  = ([],[x])
    return . Just $ FilterArg table where' values
  "created_at_lt" -> do
    _ <- tryReadDay paramKey x
    let table   = ""
    let where'  = "post_create_date < ?"
    let values  = ([],[x])
    return . Just $ FilterArg table where' values
  "created_at_gt" -> do
    _ <- tryReadDay paramKey x
    let table   = ""
    let where'  = "post_create_date > ?"
    let values  = ([],[x])
    return . Just $ FilterArg table where' values
  "category_id" -> do
    _ <- tryReadId paramKey x
    let table   = ""
    let where'  = "post_category_id = ?"
    let values  = ([],[x])
    return . Just $ FilterArg table where' values
  "tag" -> do
    _ <- tryReadId paramKey x
    let table   = "JOIN (SELECT post_id FROM poststags WHERE tag_id = ? GROUP BY post_id) AS t ON posts.post_id=t.post_id"
    let where'  = "true"
    let values  = ([x],[])
    return . Just $ FilterArg table where' values
  "tags_in" -> do
    xs <- tryReadIdArray paramKey x
    let table   = "JOIN (SELECT post_id FROM poststags WHERE tag_id IN (" ++ (init . tail . show $ xs) ++ ") GROUP BY post_id) AS t ON posts.post_id=t.post_id"
    let where'  = "true"
    let values  = ([],[])
    return . Just $ FilterArg table where' values
  "tags_all" -> do
    xs <- tryReadIdArray paramKey x
    let table   = "JOIN (SELECT post_id, array_agg(ARRAY[tag_id]) AS tags_id FROM poststags GROUP BY post_id) AS t ON posts.post_id=t.post_id"
    let where'  = "tags_id @> ARRAY" ++ show xs ++ "::bigint[]"
    let values  = ([],[])
    return . Just $ FilterArg table where' values
  "name_in" -> do
    _ <- checkLength 50 paramKey x 
    let table   = ""
    let where'  = "post_name ILIKE ?"
    let values  = ([],[Data.Text.concat ["%",escape x,"%"]])          
    return . Just $ FilterArg table where' values
  "text_in" -> do
    _ <- checkLength 50 paramKey x 
    let table   = ""
    let where'  = "post_text ILIKE ?"
    let values  = ([],[Data.Text.concat ["%",escape x,"%"]])          
    return . Just $ FilterArg table where' values
  "everywhere_in" -> do
    _ <- checkLength 50 paramKey x 
    let table   = "JOIN users AS usrs ON authors.user_id=usrs.user_id JOIN categories AS c ON c.category_id=posts.post_category_id JOIN (SELECT pt.post_id, bool_or(tag_name ILIKE ? ) AS isintag FROM poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id  GROUP BY pt.post_id) AS tg ON tg.post_id=posts.post_id"
    let where'  = "(post_text ILIKE ? OR post_name ILIKE ? OR usrs.first_name ILIKE ? OR c.category_name ILIKE ? OR isintag = TRUE)"
    let values  = ([Data.Text.concat ["%",escape x,"%"]],replicate 4 $ Data.Text.concat ["%",escape x,"%"])
    return . Just $ FilterArg table where' values
  "author_name" -> do
    _ <- checkLength 50 paramKey x 
    let table   = "JOIN users AS us ON authors.user_id=us.user_id"
    let where'  = "us.first_name = ?"
    let values  = ([],[x])
    return . Just $ FilterArg table where' values     
  _ -> throwE $ SimpleError $ "Can`t parse query parameter" ++ unpack paramKey

 

chooseSortArgsPreCheck :: (Monad m) => Request -> Text -> ExceptT ReqError m (Maybe SortArg)
chooseSortArgsPreCheck req param = case findParam req param of
    Nothing -> return Nothing
    Just (Just txt) -> chooseSortArgs txt param
    Just _ ->  throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack param

chooseSortArgs :: (Monad m) => Text -> Text -> ExceptT ReqError m (Maybe SortArg)
chooseSortArgs "DESC" param = case param of
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
    let orderBy = "true"
    return . Just $ SortArg joinTable orderBy DateDESC
  _ -> throwE $ SimpleError $ "Can`t parse query parameter: " ++ unpack param 
chooseSortArgs "ASC" param = case param of
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
    let orderBy = "true"
    return . Just $ SortArg joinTable orderBy DateASC
  _ -> throwE $ SimpleError $ "Can`t parse query parameter: " ++ unpack param 
chooseSortArgs txt param  
  | Data.Text.toUpper txt == "ASC"  = chooseSortArgs "ASC"  param
  | Data.Text.toUpper txt == "DESC" = chooseSortArgs "DESC" param
  | otherwise                       = throwE $ SimpleError $ "Invalid sort value: " ++ unpack txt ++ ". It should be only 'ASC' or 'DESC'"



findParam :: Request -> Text -> Maybe (Maybe Text)
findParam req txt = lookup txt $ queryToQueryText $ queryString req 

isExistParam :: Request -> Text -> Bool
isExistParam req txt = case findParam req txt of
  Just _  -> True
  Nothing -> False

escape :: Text -> Text
escape xs = pack $ concatMap escapeChar (unpack xs)

escapeChar :: Char -> String
escapeChar '\\' =  "\\\\" 
escapeChar '%' =  "\\%" 
escapeChar '_' =  "\\_" 
escapeChar a =  [a]