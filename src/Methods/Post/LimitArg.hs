{-# LANGUAGE OverloadedStrings #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Methods.Post.LimitArg (LimitArg (..),chooseArgs, isDateASC) where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (elemIndex, sortBy)
import Data.Text (Text, concat, pack, toUpper, unpack)
import Network.HTTP.Types.URI (queryToQueryText)
import Network.Wai (Request (..))
import Oops (ReqError (..))
import Types
import Database.PostgreSQL.Simple.Types (PGArray(..),In(..))
import Methods.Common.ToQuery (Filter(..),InF(..),CreatedF(..),TagF(..),OrderBy(..))
import Api.Request.QueryStr (GetPosts(..),GetPostsF(..),GetPostsOrd(..))


data LimitArg = LimitArg [Filter] [OrderBy]

chooseArgs (GetPosts _ gPF gPOrd) = do
  filterArgs <- chooseFilterArgs gPF
  let sortArgs = chooseSortArgs gPOrd
  return $ LimitArg filterArgs sortArgs

chooseFilterArgs (GetPostsF crAt crLt crGt tag tagsIn tagsAll nameIn textIn evIn auName catId) = do
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
  (Nothing ,Nothing ,Nothing)  -> return Nothing
  _ -> throwE $ BadReqError "Invalid combination of created filter parameters"

toTagF tag tagsIn tagsAll = case (tag,tagsIn,tagsAll) of
  (Just iD,Nothing ,Nothing)   -> return . Just $ TagF (TagIdF iD)
  (Nothing ,Just ids,Nothing)  -> return . Just $ TagF (TagsIn ids)
  (Nothing ,Nothing ,Just ids) -> return . Just $ TagF (TagsAll ids)
  (Nothing ,Nothing ,Nothing)  -> return Nothing
  _ -> throwE $ BadReqError "Invalid combination of tag filter parameters"

toInF nameIn textIn evIn = case (nameIn,textIn,evIn) of
  (Just txt,Nothing ,Nothing)  -> return . Just $ InF (Name txt)
  (Nothing ,Just txt,Nothing)  -> return . Just $ InF (PostText txt)
  (Nothing ,Nothing ,Just txt) -> return . Just $ InF $
    EveryWhere [PostText txt,Name txt,UsersName txt,CatName txt,TagName txt]
  (Nothing ,Nothing ,Nothing)  -> return Nothing
  _ -> throwE $ BadReqError "Invalid combination of IN filter parameters"

toCatIdF (Just catId) = Just $ CatIdF catId
toCatIdF _ = Nothing

toAuNameF (Just auN) = Just $ AuthorNameF auN
toAuNameF _ = Nothing 

chooseSortArgs :: GetPostsOrd -> [OrderBy]
chooseSortArgs (GetPostsOrd byPicN byCat byAu byDate) = 
  let sort1 = fmap toPicNOrd byPicN 
      sort2 = fmap toCatOrd byCat
      sort3 = fmap toAuthorOrd byAu
      sort4 = fmap toDateOrd byDate
      sortArgs = sortArgsInOrder . concatMap toList $ [sort1,sort2,sort3,sort4]
  in sortArgs

sortArgsInOrder :: [(OrderBy,Int)] -> [OrderBy]
sortArgsInOrder = fmap fst . sortBy (compare `on` snd)

toPicNOrd (sortOrd,n) = (ByPostPicsNumb sortOrd,n)
toCatOrd (sortOrd,n) = (ByPostCat sortOrd,n)
toAuthorOrd (sortOrd,n) = (ByPostAuthor sortOrd,n)
toDateOrd (sortOrd,n) = (ByPostDate sortOrd,n)

{-type SortPriority = Int

defDateSort :: SortDate
defDateSort = DateDESC-}

isDateASC :: [OrderBy] -> Bool
isDateASC = foldr (\ordBy cont -> (ordBy == ByPostDate ASC) || cont) False



{-checkReqLength :: (Monad m) => Request -> ExceptT ReqError m ()
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
-}



