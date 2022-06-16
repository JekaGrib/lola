{-# LANGUAGE TupleSections #-}

module Api.Request.QueryStr where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.List (delete, elemIndex)
import Data.Text (Text, unpack)
import Data.Time.Calendar (Day)
import Error (ReqError (..))
import Methods.Common.Exist (CheckExist (..), Handle)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Network.HTTP.Types.URI (QueryText)
import TryRead (tryReadDay, tryReadId, tryReadIdArray, tryReadPage, tryReadSortOrd)
import Types
import Prelude hiding (length)

checkQStr :: (MonadCatch m, ParseQueryStr a, CheckExist a) => Handle m -> QueryText -> ExceptT ReqError m a
checkQStr h qStr = do
  a <- parseQueryStr qStr
  checkExist h a
  return a

class (Show a) => ParseQueryStr a where
  parseQueryStr :: (Monad m) => QueryText -> ExceptT ReqError m a

data LogIn = LogIn {userIdLI :: UserId, passwordLI :: Text}
  deriving (Show)

instance ParseQueryStr LogIn where
  parseQueryStr qStr =
    LogIn
      <$> parseIdParam qStr "user_id"
      <*> parseTxtParam qStr 50 "password"

instance CheckExist LogIn where
  checkExist h (LogIn usId _) =
    checkExist h (UserId usId)

newtype Token = Token Text
  deriving (Show)

instance ParseQueryStr Token where
  parseQueryStr qStr =
    Token
      <$> parseTxtParam qStr 50 "token"

data CreateUser = CreateUser {pwdCU :: Text, fNameCU :: Text, lNameCU :: Text, picIdCU :: PictureId}
  deriving (Show)

instance ParseQueryStr CreateUser where
  parseQueryStr qStr =
    CreateUser
      <$> parseTxtParam qStr 50 "password"
      <*> parseTxtParam qStr 50 "first_name"
      <*> parseTxtParam qStr 50 "last_name"
      <*> parseIdParam qStr "user_pic_id"

instance CheckExist CreateUser where
  checkExist h (CreateUser _ _ _ picId) =
    checkExist h (PictureId picId)

newtype CreateAdminKey = CreateAdminKey {key :: Text}
  deriving (Show)

instance ParseQueryStr CreateAdminKey where
  parseQueryStr qStr =
    CreateAdminKey
      <$> parseTxtParam qStr 50 "create_admin_key"

data CreateAuthor = CreateAuthor {userIdCA :: UserId, authorInfoCA :: Text}
  deriving (Show)

instance ParseQueryStr CreateAuthor where
  parseQueryStr qStr =
    CreateAuthor
      <$> parseIdParam qStr "user_id"
      <*> parseTxtParam qStr 500 "author_info"

instance CheckExist CreateAuthor where
  checkExist h (CreateAuthor usId _) =
    checkExist h (UserId usId)

data UpdateAuthor = UpdateAuthor {userIdUA :: Id, authorInfoUA :: Text}
  deriving (Show)

instance ParseQueryStr UpdateAuthor where
  parseQueryStr qStr =
    UpdateAuthor
      <$> parseIdParam qStr "user_id"
      <*> parseTxtParam qStr 500 "author_info"

instance CheckExist UpdateAuthor where
  checkExist h (UpdateAuthor usId _) =
    checkExist h (UserId usId)

data CreateCategory
  = CreateCategory Text (Maybe Id)
  deriving (Show)

instance ParseQueryStr CreateCategory where
  parseQueryStr qStr =
    CreateCategory
      <$> parseTxtParam qStr 50 "category_name"
      <*> parseMaybeIdParam qStr "super_category_id"

instance CheckExist CreateCategory where
  checkExist h (CreateCategory _ (Just catId)) =
    checkExist h (CategoryId catId)
  checkExist _ _ = return ()

data UpdateCategory = UpdateCategory Text (Maybe Id)
  deriving (Show)

instance ParseQueryStr UpdateCategory where
  parseQueryStr qStr =
    UpdateCategory
      <$> parseTxtParam qStr 50 "category_name"
      <*> parseMaybeIdParam qStr "super_category_id"

instance CheckExist UpdateCategory where
  checkExist h (UpdateCategory _ (Just catId)) =
    checkExist h (CategoryId catId)
  checkExist _ _ = return ()

newtype CreateTag = CreateTag Text
  deriving (Show)

instance ParseQueryStr CreateTag where
  parseQueryStr qStr =
    CreateTag
      <$> parseTxtParam qStr 50 "tag_name"

instance CheckExist CreateTag where
  checkExist _ _ = return ()

newtype UpdateTag = UpdateTag Text
  deriving (Show)

instance ParseQueryStr UpdateTag where
  parseQueryStr qStr =
    UpdateTag
      <$> parseTxtParam qStr 50 "tag_name"

instance CheckExist UpdateTag where
  checkExist _ _ = return ()

newtype GetDrafts = GetDrafts Page
  deriving (Show)

instance ParseQueryStr GetDrafts where
  parseQueryStr qStr =
    GetDrafts
      <$> parsePageParam qStr "page"

instance CheckExist GetDrafts where
  checkExist _ _ = return ()

data GetPosts
  = GetPosts Page GetPostsF GetPostsOrd
  deriving (Show)

instance ParseQueryStr GetPosts where
  parseQueryStr qStr =
    GetPosts
      <$> parsePageParam qStr "page"
      <*> parseQueryStr qStr
      <*> parseQueryStr qStr

instance CheckExist GetPosts where
  checkExist h (GetPosts _ gpF _) =
    checkExist h gpF

data GetPostsF
  = GetPostsF
      (Maybe Day)
      (Maybe Day)
      (Maybe Day)
      (Maybe TagId)
      (Maybe [TagId])
      (Maybe [TagId])
      (Maybe Text)
      (Maybe Text)
      (Maybe Text)
      (Maybe AuthorName)
      (Maybe CategoryId)
  deriving (Show)

instance ParseQueryStr GetPostsF where
  parseQueryStr qStr =
    GetPostsF
      <$> parseMaybeDayParam qStr "created_at"
      <*> parseMaybeDayParam qStr "created_at_lt"
      <*> parseMaybeDayParam qStr "created_at_gt"
      <*> parseMaybeIdParam qStr "tag"
      <*> parseMaybeIdArrayParam qStr "tags_in"
      <*> parseMaybeIdArrayParam qStr "tags_all"
      <*> parseMaybeTxtParam qStr 50 "name_in"
      <*> parseMaybeTxtParam qStr 50 "text_in"
      <*> parseMaybeTxtParam qStr 50 "everywhere_in"
      <*> parseMaybeTxtParam qStr 50 "author_name"
      <*> parseMaybeIdParam qStr "category_id"

instance CheckExist GetPostsF where
  checkExist h (GetPostsF _ _ _ maybeTag maybeTagsIn maybeTagsAll _ _ _ _ maybeCat) = do
    checkExist h $ fmap TagId maybeTag
    checkExist h $ (fmap . fmap) TagId maybeTagsIn
    checkExist h $ (fmap . fmap) TagId maybeTagsAll
    checkExist h $ fmap CategoryId maybeCat

data GetPostsOrd
  = GetPostsOrd
      (Maybe (SortOrd, Int))
      (Maybe (SortOrd, Int))
      (Maybe (SortOrd, Int))
      (Maybe (SortOrd, Int))
  deriving (Show)

instance ParseQueryStr GetPostsOrd where
  parseQueryStr qStr =
    GetPostsOrd
      <$> parseMaybeSortOrdParam qStr "sort_by_pics_number"
      <*> parseMaybeSortOrdParam qStr "sort_by_category"
      <*> parseMaybeSortOrdParam qStr "sort_by_author"
      <*> parseMaybeSortOrdParam qStr "sort_by_date"

data CreateComment = CreateComment PostId Text
  deriving (Show)

instance ParseQueryStr CreateComment where
  parseQueryStr qStr =
    CreateComment
      <$> parseIdParam qStr "post_id"
      <*> parseTxtParam qStr 500 "comment_text"

instance CheckExist CreateComment where
  checkExist h (CreateComment pId _) =
    checkExist h (PostId pId)

data GetComments = GetComments Id Page
  deriving (Show)

instance ParseQueryStr GetComments where
  parseQueryStr qStr =
    GetComments
      <$> parseIdParam qStr "post_id"
      <*> parsePageParam qStr "page"

instance CheckExist GetComments where
  checkExist h (GetComments pId _) =
    checkExist h (PostId pId)

newtype UpdateComment = UpdateComment Text
  deriving (Show)

instance ParseQueryStr UpdateComment where
  parseQueryStr qStr =
    UpdateComment
      <$> parseTxtParam qStr 500 "comment_text"

instance CheckExist UpdateComment where
  checkExist _ _ = return ()

newtype LoadPicture = LoadPicture Text
  deriving (Show)

instance ParseQueryStr LoadPicture where
  parseQueryStr qStr =
    LoadPicture
      <$> parseTxtParam qStr 500 "pic_url"

instance CheckExist LoadPicture where
  checkExist _ _ = return ()

parseTxtParam :: (Monad m) => QueryText -> Int -> QueryParamKey -> ExceptT ReqError m Text
parseTxtParam qStr length paramKey = do
  paramTxt <- checkParam qStr paramKey
  checkLength length paramKey paramTxt

parseIdParam :: (Monad m) => QueryText -> QueryParamKey -> ExceptT ReqError m Id
parseIdParam qStr paramKey = do
  paramTxt <- checkParam qStr paramKey
  tryReadId paramKey paramTxt

parsePageParam :: (Monad m) => QueryText -> QueryParamKey -> ExceptT ReqError m Page
parsePageParam qStr paramKey = do
  paramTxt <- checkParam qStr paramKey
  tryReadPage paramTxt

parseMaybeTxtParam :: (Monad m) => QueryText -> Int -> QueryParamKey -> ExceptT ReqError m (Maybe Text)
parseMaybeTxtParam qStr length paramKey = do
  maybeParamTxt <- checkMaybeParam qStr paramKey
  case maybeParamTxt of
    Just paramTxt -> do
      txt <- checkLength length paramKey paramTxt
      return $ Just txt
    Nothing -> return Nothing

parseMaybeIdParam :: (Monad m) => QueryText -> QueryParamKey -> ExceptT ReqError m (Maybe Id)
parseMaybeIdParam qStr paramKey = do
  maybeParamTxt <- checkMaybeParam qStr paramKey
  case maybeParamTxt of
    Just paramTxt -> do
      num <- tryReadId paramKey paramTxt
      return (Just num)
    Nothing -> return Nothing

parseMaybeIdArrayParam :: (Monad m) => QueryText -> QueryParamKey -> ExceptT ReqError m (Maybe [Id])
parseMaybeIdArrayParam qStr paramKey = do
  maybeParamTxt <- checkMaybeParam qStr paramKey
  case maybeParamTxt of
    Just paramTxt -> do
      ids <- tryReadIdArray paramKey paramTxt
      return (Just ids)
    Nothing -> return Nothing

parseMaybeDayParam :: (Monad m) => QueryText -> QueryParamKey -> ExceptT ReqError m (Maybe Day)
parseMaybeDayParam qStr paramKey = do
  maybeParamTxt <- checkMaybeParam qStr paramKey
  case maybeParamTxt of
    Just paramTxt -> do
      day <- tryReadDay paramKey paramTxt
      return (Just day)
    Nothing -> return Nothing

parseMaybeSortOrdParam :: (Monad m) => QueryText -> QueryParamKey -> ExceptT ReqError m (Maybe (SortOrd, SortPriority))
parseMaybeSortOrdParam qStr paramKey = do
  maybeParamTxt <- checkMaybeParam qStr paramKey
  case maybeParamTxt of
    Just paramTxt -> do
      sOrd <- tryReadSortOrd paramKey paramTxt
      let index = findParamIndex qStr paramKey
      return $ fmap (sOrd,) index
    Nothing -> return Nothing

checkParam :: (Monad m) => QueryText -> QueryParamKey -> ExceptT ReqError m Text
checkParam qStr paramKey = case lookup paramKey qStr of
  Just (Just "") -> throwE $ BadReqError $ "Can't parse parameter:" ++ unpack paramKey ++ ". Empty input."
  Just (Just txt) -> checkSingleParam qStr paramKey txt
  Just Nothing -> throwE $ BadReqError $ "Can't parse parameter:" ++ unpack paramKey
  Nothing -> throwE $ BadReqError $ "Can't find parameter:" ++ unpack paramKey

checkMaybeParam :: (Monad m) => QueryText -> QueryParamKey -> ExceptT ReqError m (Maybe Text)
checkMaybeParam qStr paramKey = case lookup paramKey qStr of
  Just (Just "") -> throwE $ BadReqError $ "Can't parse parameter:" ++ unpack paramKey ++ ". Empty input."
  Just (Just txt) -> do
    checkedTxt <- checkSingleParam qStr paramKey txt
    return (Just checkedTxt)
  Just Nothing -> throwE $ BadReqError $ "Can't parse parameter:" ++ unpack paramKey
  Nothing -> return Nothing

checkSingleParam :: (Monad m) => QueryText -> QueryParamKey -> Text -> ExceptT ReqError m Text
checkSingleParam qStr paramKey txt = case lookup paramKey . delete (paramKey, Just txt) $ qStr of
  Nothing -> return txt
  Just _ -> throwE $ BadReqError $ "Multiple parameter: " ++ unpack paramKey

checkLength :: (Monad m) => Int -> QueryParamKey -> Text -> ExceptT ReqError m Text
checkLength length paramKey txt = case splitAt length (unpack txt) of
  (_, []) -> return txt
  _ -> throwE $ BadReqError $ "Parameter: " ++ unpack paramKey ++ " too long. Maximum length should be: " ++ show length

findParamIndex :: QueryText -> QueryParamKey -> Maybe Int
findParamIndex qStr paramKey = elemIndex paramKey . fmap fst $ qStr
