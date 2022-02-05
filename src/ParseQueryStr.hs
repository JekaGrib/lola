{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module ParseQueryStr   where
          
--(ParseQueryStr(..),LogIn(..),Token(..),CreateUser(..),DeleteUser(..),CreateAdmin(..),CreateAuthor(..),GetAuthor(..),UpdateAuthor(..),DeleteAuthor(..),CreateCategory(..),CreateSubCategory(..))

import Types
import           Data.Text                      ( unpack, Text )
import Oops (ReqError(..))
import           Control.Monad.Trans.Except (ExceptT,throwE)
import           Network.Wai (Request(..))
import           Network.HTTP.Types.URI         ( queryToQueryText )
import           Data.List                      ( delete )


class ParseQueryStr a where
  parseQuery :: (Monad m) => Request -> ExceptT ReqError m a



data LogIn = LogIn {user_idLI :: UserId, passwordLI :: Text}

instance ParseQueryStr LogIn where
  parseQuery req = LogIn 
    <$> parseNumParam req "user_id"
    <*> parseTxtParam req "password"

newtype Token = Token Text

instance ParseQueryStr Token where
  parseQuery req = Token
    <$>  parseTxtParam req "token"

data CreateUser = CreateUser {pwdCU :: Text, fNameCU :: Text, lNameCU :: Text, picIdCU :: PictureId}

instance ParseQueryStr CreateUser where
  parseQuery req = CreateUser
    <$> parseTxtParam req "password"
    <*> parseTxtParam req "first_name"
    <*> parseTxtParam req "last_name"
    <*> parseNumParam req "user_pic_id"

newtype DeleteUser = DeleteUser Id

instance ParseQueryStr DeleteUser where
  parseQuery req = DeleteUser
    <$>  parseNumParam req "user_id"

data CreateAdmin = CreateAdmin {keyCAK :: Text, pwdCAK :: Text, fNameCAK :: Text, lNameCAK :: Text, picIdCAK :: PictureId }

instance ParseQueryStr CreateAdmin where
  parseQuery req = CreateAdmin
    <$> parseTxtParam req "create_admin_key"
    <*> parseTxtParam req "password"
    <*> parseTxtParam req "first_name"
    <*> parseTxtParam req "last_name"
    <*> parseNumParam req "user_pic_id"

data CreateAuthor = CreateAuthor {user_idCA :: UserId, author_infoCA :: Text}

instance ParseQueryStr CreateAuthor where
  parseQuery req = CreateAuthor 
    <$> parseNumParam req "user_id"
    <*> parseTxtParam req "author_info"

newtype GetAuthor = GetAuthor Id

instance ParseQueryStr GetAuthor where
  parseQuery req = GetAuthor
    <$>  parseNumParam req "author_id"

data UpdateAuthor = UpdateAuthor {author_idUA :: Id, user_idUA :: Id, author_infoUA :: Text}

instance ParseQueryStr UpdateAuthor where
  parseQuery req = UpdateAuthor
    <$> parseNumParam req "author_id"
    <*> parseNumParam req "user_id"
    <*> parseTxtParam req "author_info"

newtype DeleteAuthor = DeleteAuthor Id

instance ParseQueryStr DeleteAuthor where
  parseQuery req = DeleteAuthor
    <$> parseNumParam req "author_id"

newtype CreateCategory = CreateCategory Text

instance ParseQueryStr CreateCategory where
  parseQuery req = CreateCategory
    <$> parseTxtParam req "category_name"

data CreateSubCategory = CreateSubCategory Text Id

instance ParseQueryStr CreateSubCategory where
  parseQuery req = CreateSubCategory
    <$> parseTxtParam req "category_name"
    <*> parseNumParam req "super_category_id"

data UpdateCategory = UpdateCategory Id Text Id

instance ParseQueryStr UpdateCategory where
  parseQuery req = UpdateCategory
    <$> parseNumParam req "category_id"
    <*> parseTxtParam req "category_name"
    <*> parseNumParam req "super_category_id"

newtype DeleteCategory = DeleteCategory Id

instance ParseQueryStr DeleteCategory where
  parseQuery req = DeleteCategory
    <$> parseNumParam req "category_id"

newtype CreateTag = CreateTag Text

instance ParseQueryStr CreateTag where
  parseQuery req = CreateTag
    <$> parseTxtParam req "tag_name"

data UpdateTag = UpdateTag Id Text

instance ParseQueryStr UpdateTag where
  parseQuery req = UpdateTag 
    <$> parseNumParam req "tag_id"
    <*> parseTxtParam req "tag_name"

newtype DeleteTag = DeleteTag Id

instance ParseQueryStr DeleteTag where
  parseQuery req = DeleteTag
    <$> parseNumParam req "tag_id"

newtype CreatePostsDraft = CreatePostsDraft Id

instance ParseQueryStr CreatePostsDraft where
  parseQuery req = CreatePostsDraft
    <$> parseNumParam req "post_id"

newtype GetDraft = GetDraft Id

instance ParseQueryStr GetDraft where
  parseQuery req = GetDraft
    <$>  parseNumParam req "draft_id"

newtype GetDrafts = GetDrafts Integer

instance ParseQueryStr GetDrafts where
  parseQuery req = GetDrafts
    <$>  parseNumParam req "page"

newtype DeleteDraft = DeleteDraft Id

instance ParseQueryStr DeleteDraft where
  parseQuery req = DeleteDraft
    <$>  parseNumParam req "draft_id"

newtype PublishDraft = PublishDraft Id

instance ParseQueryStr PublishDraft where
  parseQuery req = PublishDraft
    <$>  parseNumParam req "draft_id"

newtype DeletePost = DeletePost Id

instance ParseQueryStr DeletePost where
  parseQuery req = DeletePost
    <$>  parseNumParam req "post_id"

data CreateComment = CreateComment Id Text 

instance ParseQueryStr CreateComment where
  parseQuery req = CreateComment
    <$> parseNumParam req "post_id"
    <*> parseTxtParam req "comment_text"

data GetComments = GetComments Id Integer 

instance ParseQueryStr GetComments where
  parseQuery req = GetComments
    <$> parseNumParam req "post_id"
    <*> parseNumParam req "page"

data UpdateComment = UpdateComment Id Text 

instance ParseQueryStr UpdateComment where
  parseQuery req = UpdateComment
    <$> parseNumParam req "comment_id"
    <*> parseTxtParam req "comment_text"

newtype DeleteComment = DeleteComment Id

instance ParseQueryStr DeleteComment where
  parseQuery req = DeleteComment
    <$>  parseNumParam req "comment_id"

newtype BrowsePicture = BrowsePicture Text

instance ParseQueryStr BrowsePicture where
  parseQuery req = BrowsePicture
    <$>  parseTxtParam req "pic_url"

parseTxtParam :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m Text
parseTxtParam   = checkParam  

parseNumParam :: (Monad m) => Request -> QueryParamKey -> ExceptT ReqError m Id
parseNumParam req paramKey = do
  paramTxt <- checkParam req paramKey
  tryReadNum paramTxt


checkParam :: (Monad m) => Request -> Text -> ExceptT ReqError m Text
checkParam req paramKey = case lookup paramKey $ queryToQueryText $ queryString req of
    Just (Just "") -> throwE $ SimpleError $ "Can't parse parameter:" ++ unpack paramKey ++ ". Empty input."
    Just (Just x)  -> case lookup paramKey . delete (paramKey,Just x) $ queryToQueryText $ queryString req of
      Nothing -> return x
      Just _  -> throwE $ SimpleError $ "Multiple parameter: " ++ unpack paramKey
    Just Nothing   -> throwE $ SimpleError $ "Can't parse parameter:" ++ unpack paramKey
    Nothing        -> throwE $ SimpleError $ "Can't find parameter:" ++ unpack paramKey

tryReadNum :: (Monad m) => Text -> ExceptT ReqError m Integer
tryReadNum "" = throwE $ SimpleError "Can`t parse parameter. Empty input."
tryReadNum xs = case reads . unpack $ xs of
  [(a,"")] -> return a
  _        -> throwE $ SimpleError $ "Can`t parse value: " ++ unpack xs ++ ". It must be number"