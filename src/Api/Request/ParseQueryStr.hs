{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module ParseQueryStr where

--(ParseQueryStr(..),LogIn(..),Token(..),CreateUser(..),DeleteUser(..),CreateAdmin(..),CreateAuthor(..),GetAuthor(..),UpdateAuthor(..),DeleteAuthor(..),CreateCategory(..),CreateSubCategory(..))

import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.List (delete)
import Data.Text (Text, unpack)
import Network.HTTP.Types.URI (queryToQueryText)
import Network.Wai (Request (..))
import Oops (ReqError (..))
import TryRead (tryReadId, tryReadPage)
import Types

class (Show a) => ParseQueryStr a where
  parseQueryStr :: (Monad m) => Request -> ExceptT ReqError m a

data LogIn = LogIn {user_idLI :: UserId, passwordLI :: Text}
  deriving (Show)

instance ParseQueryStr LogIn where
  parseQueryStr req =
    LogIn
      <$> parseIdParam req "user_id"
      <*> parseTxtParam req 50 "password"

instance CheckExist LogIn where
  checkExist h (LogIn usId pwd) =
    isExistUser h "user_id" usId

newtype Token = Token Text
  deriving (Show)

instance ParseQueryStr Token where
  parseQueryStr req =
    Token
      <$> parseTxtParam req 100 "token"

data CreateUser = CreateUser {pwdCU :: Text, fNameCU :: Text, lNameCU :: Text, picIdCU :: PictureId}
  deriving (Show)

instance ParseQueryStr CreateUser where
  parseQueryStr req =
    CreateUser
      <$> parseTxtParam req 50 "password"
      <*> parseTxtParam req 50 "first_name"
      <*> parseTxtParam req 50 "last_name"
      <*> parseIdParam req "user_pic_id"

instance CheckExist LogIn where
  checkExist h (LogIn usId pwd) =
    isExistUser h "pic_id" usId

newtype DeleteUser = DeleteUser Id
  deriving (Show)

instance ParseQueryStr DeleteUser where
  parseQueryStr req =
    DeleteUser
      <$> parseIdParam req "user_id"

instance CheckExist DeleteUser where
  checkExist h (DeleteUser usId pwd) =
    isExistUser h "user_id" usId

data CreateAdmin = CreateAdmin {keyCAK :: Text, pwdCAK :: Text, fNameCAK :: Text, lNameCAK :: Text, picIdCAK :: PictureId}
  deriving (Show)

instance ParseQueryStr CreateAdmin where
  parseQueryStr req =
    CreateAdmin
      <$> parseTxtParam req 50 "create_admin_key"
      <*> parseTxtParam req 50 "password"
      <*> parseTxtParam req 50 "first_name"
      <*> parseTxtParam req 50 "last_name"
      <*> parseIdParam req "user_pic_id"

data CreateAuthor = CreateAuthor {user_idCA :: UserId, author_infoCA :: Text}
  deriving (Show)

instance ParseQueryStr CreateAuthor where
  parseQueryStr req =
    CreateAuthor
      <$> parseIdParam req "user_id"
      <*> parseTxtParam req 1000 "author_info"

newtype GetAuthor = GetAuthor Id
  deriving (Show)

instance ParseQueryStr GetAuthor where
  parseQueryStr req =
    GetAuthor
      <$> parseIdParam req "author_id"

data UpdateAuthor = UpdateAuthor {author_idUA :: Id, user_idUA :: Id, author_infoUA :: Text}
  deriving (Show)

instance ParseQueryStr UpdateAuthor where
  parseQueryStr req =
    UpdateAuthor
      <$> parseIdParam req "author_id"
      <*> parseIdParam req "user_id"
      <*> parseTxtParam req 1000 "author_info"

newtype DeleteAuthor = DeleteAuthor Id
  deriving (Show)

instance ParseQueryStr DeleteAuthor where
  parseQueryStr req =
    DeleteAuthor
      <$> parseIdParam req "author_id"

newtype CreateCategory = CreateCategory Text
  deriving (Show)

instance ParseQueryStr CreateCategory where
  parseQueryStr req =
    CreateCategory
      <$> parseTxtParam req 50 "category_name"

data CreateSubCategory = CreateSubCategory Text Id
  deriving (Show)

instance ParseQueryStr CreateSubCategory where
  parseQueryStr req =
    CreateSubCategory
      <$> parseTxtParam req 50 "category_name"
      <*> parseIdParam req "super_category_id"

data UpdateCategory = UpdateCategory Id Text (Maybe Id)
  deriving (Show)

instance ParseQueryStr UpdateCategory where
  parseQueryStr req =
    UpdateCategory
      <$> parseIdParam req "category_id"
      <*> parseTxtParam req 50 "category_name"
      <*> parseMaybeIdParam req "super_category_id"

newtype DeleteCategory = DeleteCategory Id
  deriving (Show)

instance ParseQueryStr DeleteCategory where
  parseQueryStr req =
    DeleteCategory
      <$> parseIdParam req "category_id"

newtype CreateTag = CreateTag Text
  deriving (Show)

instance ParseQueryStr CreateTag where
  parseQueryStr req =
    CreateTag
      <$> parseTxtParam req 50 "tag_name"

data UpdateTag = UpdateTag Id Text
  deriving (Show)

instance ParseQueryStr UpdateTag where
  parseQueryStr req =
    UpdateTag
      <$> parseIdParam req "tag_id"
      <*> parseTxtParam req 50 "tag_name"

newtype DeleteTag = DeleteTag Id
  deriving (Show)

instance ParseQueryStr DeleteTag where
  parseQueryStr req =
    DeleteTag
      <$> parseIdParam req "tag_id"

newtype CreatePostsDraft = CreatePostsDraft Id
  deriving (Show)

instance ParseQueryStr CreatePostsDraft where
  parseQueryStr req =
    CreatePostsDraft
      <$> parseIdParam req "post_id"

newtype GetDraft = GetDraft Id
  deriving (Show)

instance ParseQueryStr GetDraft where
  parseQueryStr req =
    GetDraft
      <$> parseIdParam req "draft_id"

newtype GetDrafts = GetDrafts Page
  deriving (Show)

instance ParseQueryStr GetDrafts where
  parseQueryStr req =
    GetDrafts
      <$> parsePageParam req "page"

newtype DeleteDraft = DeleteDraft Id
  deriving (Show)

instance ParseQueryStr DeleteDraft where
  parseQueryStr req =
    DeleteDraft
      <$> parseIdParam req "draft_id"

newtype PublishDraft = PublishDraft Id
  deriving (Show)

instance ParseQueryStr PublishDraft where
  parseQueryStr req =
    PublishDraft
      <$> parseIdParam req "draft_id"

newtype DeletePost = DeletePost Id
  deriving (Show)

instance ParseQueryStr DeletePost where
  parseQueryStr req =
    DeletePost
      <$> parseIdParam req "post_id"

data GetPosts = 
  GetPosts GetPostsF GetPostsOrd

data GetPostsF = 
  GetPostsF 
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
  parseQueryStr req =
    GetPostsF
      <$> parseMaybeDayParam      req "crated_at"
      <*> parseMaybeDayParam      req "crated_at_lt"
      <*> parseMaybeDayParam      req "crated_at_gt"
      <*> parseMaybeIdParam       req "tag"
      <*> parseMaybeIdArrayParam  req "tags_in"
      <*> parseMaybeIdArrayParam  req "tags_all"
      <*> parseMaybeTxtParam   req 50 "name_in"
      <*> parseMaybeTxtParam   req 50 "text_in"
      <*> parseMaybeTxtParam   req 50 "everywhere_in"
      <*> parseMaybeTxtParam   req 50 "author_name"
      <*> parseMaybeIdParam       req "category_id"


data GetPostsOrd = 
  GetPostsOrd 
    Maybe (SortOrd,Int)
    Maybe (SortOrd,Int)
    Maybe (SortOrd,Int)
    Maybe (SortOrd,Int)
  deriving (Show)

data CreateComment = CreateComment Id Text
  deriving (Show)

instance ParseQueryStr CreateComment where
  parseQueryStr req =
    CreateComment
      <$> parseMaybeSortOrdParam req "sort_by_pics_number"
      <*> parseMaybeSortOrdParam req "sort_by_category"
      <*> parseMaybeSortOrdParam req "sort_by_author"
      <*> parseMaybeSortOrdParam req "sort_by_date"

data GetComments = GetComments Id Page
  deriving (Show)

instance ParseQueryStr GetComments where
  parseQueryStr req =
    GetComments
      <$> parseIdParam req "post_id"
      <*> parsePageParam req "page"

data UpdateComment = UpdateComment Id Text
  deriving (Show)

instance ParseQueryStr UpdateComment where
  parseQueryStr req =
    UpdateComment
      <$> parseIdParam req "comment_id"
      <*> parseTxtParam req 1000 "comment_text"

newtype DeleteComment = DeleteComment Id
  deriving (Show)

instance ParseQueryStr DeleteComment where
  parseQueryStr req =
    DeleteComment
      <$> parseIdParam req "comment_id"

newtype BrowsePicture = BrowsePicture Text
  deriving (Show)

instance ParseQueryStr BrowsePicture where
  parseQueryStr req =
    BrowsePicture
      <$> parseTxtParam req 500 "pic_url"

parseTxtParam :: (Monad m) => Request -> Int -> QueryParamKey -> ExceptT ReqError m Text
parseTxtParam req leng paramKey = do
  paramTxt <- checkParam req paramKey
  checkLength leng paramKey paramTxt

parseIdParam :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m Id
parseIdParam req paramKey = do
  paramTxt <- checkParam req paramKey
  tryReadId paramKey paramTxt

parsePageParam :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m Page
parsePageParam req paramKey = do
  paramTxt <- checkParam req paramKey
  tryReadPage paramTxt

parseMaybeTxtParam :: (Monad m) => Request -> Int -> QueryParamKey -> ExceptT ReqError m (Maybe Text)
parseMaybeTxtParam req leng paramKey = do
  maybeParamTxt <- checkMaybeParam req paramKey
  case maybeParamTxt of
    Just paramTxt -> do
      txt <- checkLength leng paramKey paramTxt
      return $ Just txt
    Nothing -> return Nothing

parseMaybeIdParam :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m (Maybe Id)
parseMaybeIdParam req paramKey = do
  maybeParamTxt <- checkMaybeParam req paramKey
  case maybeParamTxt of
    Just paramTxt -> do
      num <- tryReadId paramKey paramTxt
      return (Just num)
    Nothing -> return Nothing

parseMaybeIdArrayParam :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m (Maybe [Id])
parseMaybeIdArrayParam req paramKey = do
  maybeParamTxt <- checkMaybeParam req paramKey
  case maybeParamTxt of
    Just paramTxt -> do
      ids <- tryReadIdArray paramKey paramTxt
      return (Just ids)
    Nothing -> return Nothing

parseMaybeDayParam :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m (Maybe Day)
parseMaybeDayParam req paramKey = do
  maybeParamTxt <- checkMaybeParam req paramKey
  case maybeParamTxt of
    Just paramTxt -> do
      day <- tryReadDay paramKey paramTxt
      return (Just day)
    Nothing -> return Nothing

parseMaybeSortOrdParam :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m (Maybe (SortOrd,Int))
parseMaybeSortOrdParam req paramKey = do
  maybeParamTxt <- checkMaybeParam req paramKey
  case maybeParamTxt of
    Just paramTxt -> do
      sOrd <- tryReadSortOrd paramKey paramTxt
      ind <- findParamIndex req paramKey 
      return (Just (sOrd,ind))
    Nothing -> return Nothing

checkParam :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m Text
checkParam req paramKey = case lookup paramKey $ queryToQueryText $ queryString req of
  Just (Just "") -> throwE $ SimpleError $ "Can't parse parameter:" ++ unpack paramKey ++ ". Empty input."
  Just (Just txt) -> checkSingleParam req paramKey txt
  Just Nothing -> throwE $ SimpleError $ "Can't parse parameter:" ++ unpack paramKey
  Nothing -> throwE $ SimpleError $ "Can't find parameter:" ++ unpack paramKey

checkMaybeParam :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m (Maybe Text)
checkMaybeParam req paramKey = case lookup paramKey $ queryToQueryText $ queryString req of
  Just (Just "") -> throwE $ SimpleError $ "Can't parse parameter:" ++ unpack paramKey ++ ". Empty input."
  Just (Just txt) -> do
    txt1 <- checkSingleParam req paramKey txt
    return (Just txt1)
  Just Nothing -> throwE $ SimpleError $ "Can't parse parameter:" ++ unpack paramKey
  Nothing -> return Nothing

checkSingleParam :: (Monad m) => Request -> QueryParamKey -> Text -> ExceptT ReqError m Text
checkSingleParam req paramKey txt = case lookup paramKey . delete (paramKey, Just txt) $ queryToQueryText $ queryString req of
  Nothing -> return txt
  Just _ -> throwE $ SimpleError $ "Multiple parameter: " ++ unpack paramKey

checkLength :: (Monad m) => Int -> QueryParamKey -> Text -> ExceptT ReqError m Text
checkLength leng paramKey txt = case splitAt leng (unpack txt) of
  (_, []) -> return txt
  _ -> throwE $ SimpleError $ "Parameter: " ++ unpack paramKey ++ " too long. Maximum length should be: " ++ show leng

findParamIndex :: Request -> QueryParamKey -> Maybe Int
findParamIndex req paramKey = elemIndex paramKey . fmap fst . queryToQueryText $ queryString req