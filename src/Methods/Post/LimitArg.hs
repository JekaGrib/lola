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

chooseArgs (GetPosts _ gPF gPOrd) =
  let filterArgs = chooseFilterArgs gPF
  let sortArgs = chooseSortArgs gPOrd
  return $ LimitArg filterArgs sortArgs

chooseFilterArgs (GetPostsF crAt crLt crGt tag tagsIn tagsAll nameIn textIn evIn catId auName) = do
  filt1 <- toCreatedF crAt crLt crGt
  filt2 <- toTagF tag tagsIn tagsAll
  filt3 <- toInF nameIn textIn evIn
  let filt4 = toCatIdF catId
  let filt5 = toAuNameF auName
  let filterArgs = concatMap toList [filt1,filt2,filt3,filt4,filt5]
  return filterArgs

toCreatedF crAt crLt crGt = case (crAt,crLt,crGt) of
  (Just day,Nothing ,Nothing)  -> return . Just $ CreatedF (At day)
  (Nothing ,Just day,Nothing)  -> return . Just $ CreatedF (AtLt day)
  (Nothing ,Nothing ,Just day) -> return . Just $ CreatedF (AtGt day)
  (Nothing ,Nothing ,Nothing)  -> return empty
  _ -> throwE $ SimpleError "Invalid combination of filter parameters"

toTagF tag tagsIn tagsAll = case (tag,tagsIn,tagsAll) of
  (Just iD,Nothing ,Nothing)   -> return . Just $ TagF (TagId iD)
  (Nothing ,Just ids,Nothing)  -> return . Just $ TagF (TagsIn ids)
  (Nothing ,Nothing ,Just ids) -> return . Just $ TagF (TagsAll ids)
  (Nothing ,Nothing ,Nothing)  -> return empty
  _ -> throwE $ SimpleError "Invalid combination of filter parameters"

toInF nameIn textIn evIn = case (nameIn,textIn,evIn) of
  (Just txt,Nothing ,Nothing)  -> return . Just $ InF (Name (escape txt))
  (Nothing ,Just txt,Nothing)  -> return . Just $ InF (PostText (escape txt))
  (Nothing ,Nothing ,Just txt) -> return . Just $ InF $
    let escTxt = escape txt  
    in EveryWhere [PostText escTxt,Name escTxt,UsersName escTxt,CatName escTxt,TagName escTxt]
  (Nothing ,Nothing ,Nothing)  -> return empty
  _ -> throwE $ SimpleError "Invalid combination of filter parameters"

toCatIdF (Just catId) = Just $ CatIdF catId
toCatIdF _ = empty

toAuNameF (Just auN) = Just $ AuthorNameF auName
toAuNameF _ = empty 

chooseSortArgs (GetPostsOrd byPicN byCat byAu byDate) = do
  let sort1 = fmap toPicNOrd byPicN
  let sort2 = fmap toCatOrd byCat
  let sort3 = fmap toAuthorOrd byAu
  let sort4 = fmap toDateOrd byDate
  let sortArgs = checkSortPriority . concatMap toList [sort1,sort2,sort3,sort4]

checkSortPriority = sortBy (compare `on` snd)

toPicNOrd (sortOrd,n) = (ByPostPicsNumb sortOrd,n)
toCatOrd (sortOrd,n) = (ByPostCat sortOrd,n)
toAuthorOrd (sortOrd,n) = (ByPostAuthor sortOrd,n)
toDateOrd (sortOrd,n) = (ByPostDate sortOrd,n)

type SortPriority = Int

defDateSort :: SortDate
defDateSort = DateDESC

isDateASC :: [OrderBy] -> Bool
isDateASC = foldr (\ordBy cont -> (ordBy == ByPostDate ACS) || cont) False



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


data Filter = 
  CreatedF CreatedF
  | CatIdF CategoryId
  | TagF TagF
  | InF InF
  | AuthorNameF Text

data CreatedF = 
  At Day
  | AtLt Day
  | AtGt Day

data TagF =
  | TagId TagId
  | TagsIn [TagId]
  | TagsAll [TagId]

data InF =
  | PostText Text
  | Name Text
  | UsersName Text
  | CatName Text
  | TagName Text
  | EveryWhere [InF]

class ToWhere a where
  toWhere :: a -> Where

instance InF ToWhere where
  toWhere (PostText txt) = 
    WherePair " post_text ILIKE %?% " (Txt (escape txt))
  toWhere (Name txt) = 
    WherePair " post_name ILIKE %?% " (Txt (escape txt))
  toWhere (UsersName txt) = 
    WherePair " usrs.first_name ILIKE %?% " (Txt (escape txt))
  toWhere (TagName txt) =
    let sel = Select 
      ["post_id"] 
      "tags JOIN poststags AS pt ON tags.tag_id = pt.tag_id" 
      (WherePair " tag_name ILIKE %?% " (Txt (escape txt)))
    in WhereSelect " posts.post_id IN " sel
  toWhere (CatName txt) =
    WherePair " c.category_name ILIKE %?% " (Txt (escape txt))
  toWhere EveryWhere xs = WhereOr (fmap toWhere xs)
  

instance AddJoinTable InF where
  addJoinTable (UsersName _) = " JOIN users AS us ON authors.user_id=us.user_id"
  addJoinTable (CatName _) = " JOIN users AS us ON authors.user_id=us.user_id"
  addJoinTable _ = " JOIN categories AS c ON c.category_id=posts.post_category_id"

instance CreatedF ToWhere where
  toWhere (At day) = 
    WherePair " post_create_date = ? " (Day day)
  toWhere (AtLt day) = 
    WherePair " post_create_date < ? " (Day day)
  toWhere (AtGt day) = 
    WherePair " post_create_date > ? " (Day day)

instance TagF ToWhere where
  toWhere (TadId iD) = 
    let sel = Select
      ["post_id"] 
      "poststags"
       WherePair " tag_id = ? " (Id iD)
    in WhereSelect " posts.post_id IN " sel
  toWhere (TadsIn idS) = 
    let sel = Select
      ["post_id"] 
      "poststags"
       WherePair " tag_id IN ? " (IdIn (In idS))
    in WhereSelect " posts.post_id IN " sel
  toWhere (TadsAll idS) = 
    let sel = Select
      ["array_agg(tag_id)"] 
      "poststags"
       Where "posts.post_id = poststags.post_id"
    in WhereSelectPair sel " @>?::bigint[] " (IdArray (PGArray idS))
