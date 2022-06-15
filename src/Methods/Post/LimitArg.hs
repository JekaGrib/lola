module Methods.Post.LimitArg (LimitArg (..), chooseArgs, isDateASC) where

import Api.Request.QueryStr (GetPosts (..), GetPostsF (..), GetPostsOrd (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (sortBy)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Error (ReqError (..))
import Psql.ToQuery.SelectLimit (CreatedF (..), Filter (..), InF (..), OrderBy (..), TagF (..))
import Types

data LimitArg = LimitArg [Filter] [OrderBy]

chooseArgs :: (MonadCatch m) => GetPosts -> ExceptT ReqError m LimitArg
chooseArgs (GetPosts _ gPF gPOrd) = do
  filterArgs <- chooseFilterArgs gPF
  let sortArgs = chooseSortArgs gPOrd
  return $ LimitArg filterArgs sortArgs

chooseFilterArgs :: (MonadCatch m) => GetPostsF -> ExceptT ReqError m [Filter]
chooseFilterArgs (GetPostsF crAt crLt crGt tag tagsIn tagsAll nameIn textIn evIn auName catId) = do
  filterCreated <- toCreatedF crAt crLt crGt
  filterTag <- toTagF tag tagsIn tagsAll
  filterIn <- toInF nameIn textIn evIn
  let filterCat = toCatIdF catId
  let filtAuthor = toAuNameF auName
  let filterArgs = concatMap toList [filterCreated, filterTag, filterIn, filterCat, filtAuthor]
  return filterArgs

toCreatedF :: (MonadCatch m) => Maybe Day -> Maybe Day -> Maybe Day -> ExceptT ReqError m (Maybe Filter)
toCreatedF crAt crLt crGt = case (crAt, crLt, crGt) of
  (Just day, Nothing, Nothing) -> return . Just $ CreatedF (At day)
  (Nothing, Just day, Nothing) -> return . Just $ CreatedF (AtLt day)
  (Nothing, Nothing, Just day) -> return . Just $ CreatedF (AtGt day)
  (Nothing, Nothing, Nothing) -> return Nothing
  _ -> throwE $ BadReqError "Invalid combination of created filter parameters"

toTagF :: (MonadCatch m) => Maybe TagId -> Maybe [TagId] -> Maybe [TagId] -> ExceptT ReqError m (Maybe Filter)
toTagF tag tagsIn tagsAll = case (tag, tagsIn, tagsAll) of
  (Just iD, Nothing, Nothing) -> return . Just $ TagF (TagIdF iD)
  (Nothing, Just ids, Nothing) -> return . Just $ TagF (TagsIn ids)
  (Nothing, Nothing, Just ids) -> return . Just $ TagF (TagsAll ids)
  (Nothing, Nothing, Nothing) -> return Nothing
  _ -> throwE $ BadReqError "Invalid combination of tag filter parameters"

toInF :: (MonadCatch m) => Maybe Text -> Maybe Text -> Maybe Text -> ExceptT ReqError m (Maybe Filter)
toInF nameIn textIn evIn = case (nameIn, textIn, evIn) of
  (Just txt, Nothing, Nothing) -> return . Just $ InF (Name txt)
  (Nothing, Just txt, Nothing) -> return . Just $ InF (PostText txt)
  (Nothing, Nothing, Just txt) ->
    return . Just $ InF $
      EveryWhere [PostText txt, Name txt, UsersName txt, CatName txt, TagName txt]
  (Nothing, Nothing, Nothing) -> return Nothing
  _ -> throwE $ BadReqError "Invalid combination of IN filter parameters"

toCatIdF :: Maybe CategoryId -> Maybe Filter
toCatIdF = fmap CatIdF

toAuNameF :: Maybe AuthorName -> Maybe Filter
toAuNameF = fmap AuthorNameF

chooseSortArgs :: GetPostsOrd -> [OrderBy]
chooseSortArgs (GetPostsOrd byPicN byCat byAu byDate) =
  let sortPic = fmap toPicNOrd byPicN
      sortCat = fmap toCatOrd byCat
      sortAuthor = fmap toAuthorOrd byAu
      sortDate = fmap toDateOrd byDate
      sortArgs = sortArgsInOrder . concatMap toList $ [sortPic, sortCat, sortAuthor, sortDate]
   in sortArgs

sortArgsInOrder :: [(OrderBy, SortPriority)] -> [OrderBy]
sortArgsInOrder = fmap fst . sortBy (compare `on` snd)

toPicNOrd :: (SortOrd, Int) -> (OrderBy, SortPriority)
toPicNOrd (sortOrd, n) = (ByPostPicsNumb sortOrd, n)

toCatOrd :: (SortOrd, Int) -> (OrderBy, SortPriority)
toCatOrd (sortOrd, n) = (ByPostCat sortOrd, n)

toAuthorOrd :: (SortOrd, Int) -> (OrderBy, SortPriority)
toAuthorOrd (sortOrd, n) = (ByPostAuthor sortOrd, n)

toDateOrd :: (SortOrd, Int) -> (OrderBy, SortPriority)
toDateOrd (sortOrd, n) = (ByPostDate sortOrd, n)

isDateASC :: [OrderBy] -> Bool
isDateASC = foldr (\ordBy cont -> (ordBy == ByPostDate ASC) || cont) False
