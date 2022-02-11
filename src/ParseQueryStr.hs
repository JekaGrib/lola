{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module ParseQueryStr   where
          
--(ParseQueryStr(..),LogIn(..),Token(..),CreateUser(..),DeleteUser(..),CreateAdmin(..),CreateAuthor(..),GetAuthor(..),UpdateAuthor(..),DeleteAuthor(..),CreateCategory(..),CreateSubCategory(..))

import Types
import TryRead (tryReadNumKey)
import           Data.Text                      ( unpack, Text )
import Oops (ReqError(..))
import           Control.Monad.Trans.Except (ExceptT,throwE)
import           Network.Wai (Request(..))
import           Network.HTTP.Types.URI         ( queryToQueryText )
import           Data.List                      ( delete )


class (Show a) => ParseQueryStr a where
  parseQueryStr :: (Monad m) => Request -> ExceptT ReqError m a



data LogIn = LogIn {user_idLI :: UserId, passwordLI :: Text}
 deriving Show

instance ParseQueryStr LogIn where
  parseQueryStr req = LogIn 
    <$> parseNumParam req "user_id"
    <*> parseTxtParam req 50 "password"

newtype Token = Token Text
 deriving Show


instance ParseQueryStr Token where
  parseQueryStr req = Token
    <$>  parseTxtParam req 100 "token"

data CreateUser = CreateUser {pwdCU :: Text, fNameCU :: Text, lNameCU :: Text, picIdCU :: PictureId}
 deriving Show

instance ParseQueryStr CreateUser where
  parseQueryStr req = CreateUser
    <$> parseTxtParam req 50 "password"
    <*> parseTxtParam req 50 "first_name"
    <*> parseTxtParam req 50 "last_name"
    <*> parseNumParam req "user_pic_id"

newtype DeleteUser = DeleteUser Id
 deriving Show

instance ParseQueryStr DeleteUser where
  parseQueryStr req = DeleteUser
    <$>  parseNumParam req "user_id"

data CreateAdmin = CreateAdmin {keyCAK :: Text, pwdCAK :: Text, fNameCAK :: Text, lNameCAK :: Text, picIdCAK :: PictureId }
 deriving Show

instance ParseQueryStr CreateAdmin where
  parseQueryStr req = CreateAdmin
    <$> parseTxtParam req 50 "create_admin_key"
    <*> parseTxtParam req 50 "password"
    <*> parseTxtParam req 50 "first_name"
    <*> parseTxtParam req 50 "last_name"
    <*> parseNumParam req "user_pic_id"

data CreateAuthor = CreateAuthor {user_idCA :: UserId, author_infoCA :: Text}
 deriving Show

instance ParseQueryStr CreateAuthor where
  parseQueryStr req = CreateAuthor 
    <$> parseNumParam req "user_id"
    <*> parseTxtParam req 1000 "author_info"

newtype GetAuthor = GetAuthor Id
 deriving Show

instance ParseQueryStr GetAuthor where
  parseQueryStr req = GetAuthor
    <$>  parseNumParam req "author_id"

data UpdateAuthor = UpdateAuthor {author_idUA :: Id, user_idUA :: Id, author_infoUA :: Text}
 deriving Show

instance ParseQueryStr UpdateAuthor where
  parseQueryStr req = UpdateAuthor
    <$> parseNumParam req "author_id"
    <*> parseNumParam req "user_id"
    <*> parseTxtParam req 1000 "author_info"

newtype DeleteAuthor = DeleteAuthor Id
 deriving Show

instance ParseQueryStr DeleteAuthor where
  parseQueryStr req = DeleteAuthor
    <$> parseNumParam req "author_id"

newtype CreateCategory = CreateCategory Text
 deriving Show

instance ParseQueryStr CreateCategory where
  parseQueryStr req = CreateCategory
    <$> parseTxtParam req 50 "category_name"

data CreateSubCategory = CreateSubCategory Text Id
 deriving Show

instance ParseQueryStr CreateSubCategory where
  parseQueryStr req = CreateSubCategory
    <$> parseTxtParam req 50 "category_name"
    <*> parseNumParam req "super_category_id"

data UpdateCategory = UpdateCategory Id Text (Maybe Id)
 deriving Show

instance ParseQueryStr UpdateCategory where
  parseQueryStr req = UpdateCategory
    <$> parseNumParam req "category_id"
    <*> parseTxtParam req 50 "category_name"
    <*> parseMaybeNumParam req "super_category_id"

newtype DeleteCategory = DeleteCategory Id
 deriving Show

instance ParseQueryStr DeleteCategory where
  parseQueryStr req = DeleteCategory
    <$> parseNumParam req "category_id"

newtype CreateTag = CreateTag Text
 deriving Show

instance ParseQueryStr CreateTag where
  parseQueryStr req = CreateTag
    <$> parseTxtParam req 50 "tag_name"

data UpdateTag = UpdateTag Id Text
 deriving Show

instance ParseQueryStr UpdateTag where
  parseQueryStr req = UpdateTag 
    <$> parseNumParam req "tag_id"
    <*> parseTxtParam req 50 "tag_name"

newtype DeleteTag = DeleteTag Id
 deriving Show

instance ParseQueryStr DeleteTag where
  parseQueryStr req = DeleteTag
    <$> parseNumParam req "tag_id"

newtype CreatePostsDraft = CreatePostsDraft Id
 deriving Show

instance ParseQueryStr CreatePostsDraft where
  parseQueryStr req = CreatePostsDraft
    <$> parseNumParam req "post_id"

newtype GetDraft = GetDraft Id
 deriving Show

instance ParseQueryStr GetDraft where
  parseQueryStr req = GetDraft
    <$>  parseNumParam req "draft_id"

newtype GetDrafts = GetDrafts Integer
 deriving Show

instance ParseQueryStr GetDrafts where
  parseQueryStr req = GetDrafts
    <$>  parseNumParam req "page"

newtype DeleteDraft = DeleteDraft Id
 deriving Show

instance ParseQueryStr DeleteDraft where
  parseQueryStr req = DeleteDraft
    <$>  parseNumParam req "draft_id"

newtype PublishDraft = PublishDraft Id
 deriving Show

instance ParseQueryStr PublishDraft where
  parseQueryStr req = PublishDraft
    <$>  parseNumParam req "draft_id"

newtype DeletePost = DeletePost Id
 deriving Show

instance ParseQueryStr DeletePost where
  parseQueryStr req = DeletePost
    <$>  parseNumParam req "post_id"

data CreateComment = CreateComment Id Text 
 deriving Show

instance ParseQueryStr CreateComment where
  parseQueryStr req = CreateComment
    <$> parseNumParam req "post_id"
    <*> parseTxtParam req 1000 "comment_text"

data GetComments = GetComments Id Integer 
 deriving Show

instance ParseQueryStr GetComments where
  parseQueryStr req = GetComments
    <$> parseNumParam req "post_id"
    <*> parseNumParam req "page"

data UpdateComment = UpdateComment Id Text 
 deriving Show

instance ParseQueryStr UpdateComment where
  parseQueryStr req = UpdateComment
    <$> parseNumParam req "comment_id"
    <*> parseTxtParam req 1000 "comment_text"

newtype DeleteComment = DeleteComment Id
 deriving Show

instance ParseQueryStr DeleteComment where
  parseQueryStr req = DeleteComment
    <$>  parseNumParam req "comment_id"

newtype BrowsePicture = BrowsePicture Text
 deriving Show

instance ParseQueryStr BrowsePicture where
  parseQueryStr req = BrowsePicture
    <$>  parseTxtParam req 500 "pic_url"



parseTxtParam :: (Monad m) =>  Request -> Int -> QueryParamKey -> ExceptT ReqError m Text
parseTxtParam req leng paramKey = do
  paramTxt <- checkParam req paramKey
  checkLength leng paramKey paramTxt


parseNumParam :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m Id
parseNumParam req paramKey = do
  paramTxt <- checkParam req paramKey
  tryReadNumKey paramTxt paramKey

parseMaybeNumParam :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m (Maybe Id)
parseMaybeNumParam req paramKey = do
  maybeParamTxt <- checkMaybeParam req paramKey
  case maybeParamTxt of
    Just paramTxt -> do
      num <- tryReadNumKey paramTxt paramKey
      return (Just num)
    Nothing -> return Nothing

checkParam :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m Text
checkParam req paramKey = case lookup paramKey $ queryToQueryText $ queryString req of
    Just (Just "") -> throwE $ SimpleError $ "Can't parse parameter:" ++ unpack paramKey ++ ". Empty input."
    Just (Just txt)  -> case lookup paramKey . delete (paramKey,Just txt) $ queryToQueryText $ queryString req of
      Nothing -> return txt
      Just _  -> throwE $ SimpleError $ "Multiple parameter: " ++ unpack paramKey
    Just Nothing   -> throwE $ SimpleError $ "Can't parse parameter:" ++ unpack paramKey
    Nothing        -> throwE $ SimpleError $ "Can't find parameter:" ++ unpack paramKey

checkMaybeParam :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m (Maybe Text)
checkMaybeParam req paramKey = case lookup paramKey $ queryToQueryText $ queryString req of
    Just (Just "") -> throwE $ SimpleError $ "Can't parse parameter:" ++ unpack paramKey ++ ". Empty input."
    Just (Just txt)  -> case lookup paramKey . delete (paramKey,Just txt) $ queryToQueryText $ queryString req of
      Nothing -> return (Just txt)
      Just _  -> throwE $ SimpleError $ "Multiple parameter: " ++ unpack paramKey
    Just Nothing   -> throwE $ SimpleError $ "Can't parse parameter:" ++ unpack paramKey
    Nothing        -> return Nothing

checkLength :: (Monad m) => Int -> QueryParamKey -> Text -> ExceptT ReqError m Text
checkLength leng paramKey txt = do
  if (length . unpack $ txt) > leng 
    then throwE $ SimpleError $ "Parameter: " ++ unpack paramKey ++ " too long. Maximum length should be: " ++ show leng
    else return txt

