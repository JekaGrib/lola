{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Post.LimitArg (LimitArg (..),chooseArgs, isDateASC) where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (sortBy)
import Data.Text (Text)
import Oops (ReqError (..))
import Types
import Psql.ToQuery.SelectLimit (Filter(..),InF(..),CreatedF(..),TagF(..),OrderBy(..))
import Api.Request.QueryStr (GetPosts(..),GetPostsF(..),GetPostsOrd(..))
import Control.Monad.Catch (MonadCatch)
import Data.Time.Calendar ( Day)


data LimitArg = LimitArg [Filter] [OrderBy]

chooseArgs :: (MonadCatch m) => GetPosts -> ExceptT ReqError m LimitArg
chooseArgs (GetPosts _ gPF gPOrd) = do
  filterArgs <- chooseFilterArgs gPF
  let sortArgs = chooseSortArgs gPOrd
  return $ LimitArg filterArgs sortArgs

chooseFilterArgs :: (MonadCatch m) => GetPostsF -> ExceptT ReqError m [Filter]
chooseFilterArgs (GetPostsF crAt crLt crGt tag tagsIn tagsAll nameIn textIn evIn auName catId) = do
  filt1 <- toCreatedF crAt crLt crGt
  filt2 <- toTagF tag tagsIn tagsAll
  filt3 <- toInF nameIn textIn evIn
  let filt4 = toCatIdF catId
  let filt5 = toAuNameF auName
  let filterArgs = concatMap toList [filt1,filt2,filt3,filt4,filt5]
  return filterArgs

toCreatedF :: (MonadCatch m) => Maybe Day -> Maybe Day -> Maybe Day -> ExceptT ReqError m (Maybe Filter)
toCreatedF crAt crLt crGt = case (crAt,crLt,crGt) of
  (Just day,Nothing ,Nothing)  -> return . Just $ CreatedF (At day)
  (Nothing ,Just day,Nothing)  -> return . Just $ CreatedF (AtLt day)
  (Nothing ,Nothing ,Just day) -> return . Just $ CreatedF (AtGt day)
  (Nothing ,Nothing ,Nothing)  -> return Nothing
  _ -> throwE $ BadReqError "Invalid combination of created filter parameters"

toTagF :: (MonadCatch m) => Maybe TagId -> Maybe [TagId] -> Maybe [TagId] -> ExceptT ReqError m (Maybe Filter)
toTagF tag tagsIn tagsAll = case (tag,tagsIn,tagsAll) of
  (Just iD,Nothing ,Nothing)   -> return . Just $ TagF (TagIdF iD)
  (Nothing ,Just ids,Nothing)  -> return . Just $ TagF (TagsIn ids)
  (Nothing ,Nothing ,Just ids) -> return . Just $ TagF (TagsAll ids)
  (Nothing ,Nothing ,Nothing)  -> return Nothing
  _ -> throwE $ BadReqError "Invalid combination of tag filter parameters"

toInF :: (MonadCatch m) => Maybe Text -> Maybe Text -> Maybe Text -> ExceptT ReqError m (Maybe Filter)
toInF nameIn textIn evIn = case (nameIn,textIn,evIn) of
  (Just txt,Nothing ,Nothing)  -> return . Just $ InF (Name txt)
  (Nothing ,Just txt,Nothing)  -> return . Just $ InF (PostText txt)
  (Nothing ,Nothing ,Just txt) -> return . Just $ InF $
    EveryWhere [PostText txt,Name txt,UsersName txt,CatName txt,TagName txt]
  (Nothing ,Nothing ,Nothing)  -> return Nothing
  _ -> throwE $ BadReqError "Invalid combination of IN filter parameters"

toCatIdF :: Maybe CategoryId -> Maybe Filter
toCatIdF maybeCatId = fmap CatIdF maybeCatId

toAuNameF :: Maybe AuthorName -> Maybe Filter
toAuNameF maybeAuN = fmap AuthorNameF maybeAuN

chooseSortArgs :: GetPostsOrd -> [OrderBy]
chooseSortArgs (GetPostsOrd byPicN byCat byAu byDate) = 
  let sort1 = fmap toPicNOrd byPicN 
      sort2 = fmap toCatOrd byCat
      sort3 = fmap toAuthorOrd byAu
      sort4 = fmap toDateOrd byDate
      sortArgs = sortArgsInOrder . concatMap toList $ [sort1,sort2,sort3,sort4]
  in sortArgs

sortArgsInOrder :: [(OrderBy,SortPriority)] -> [OrderBy]
sortArgsInOrder = fmap fst . sortBy (compare `on` snd)

toPicNOrd :: (SortOrd,Int) -> (OrderBy,SortPriority)
toPicNOrd (sortOrd,n) = (ByPostPicsNumb sortOrd,n)

toCatOrd :: (SortOrd,Int) -> (OrderBy,SortPriority)
toCatOrd (sortOrd,n) = (ByPostCat sortOrd,n)

toAuthorOrd :: (SortOrd,Int) -> (OrderBy,SortPriority)
toAuthorOrd (sortOrd,n) = (ByPostAuthor sortOrd,n)

toDateOrd :: (SortOrd,Int) -> (OrderBy,SortPriority)
toDateOrd (sortOrd,n) = (ByPostDate sortOrd,n)



isDateASC :: [OrderBy] -> Bool
isDateASC = foldr (\ordBy cont -> (ordBy == ByPostDate ASC) || cont) False

{-type SortPriority = Int

defDateSort :: SortDate
defDateSort = DateDESC-}


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



