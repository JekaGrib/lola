{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Api.Request.QueryStr where

--(ParseQueryStr(..),LogIn(..),Token(..),CreateUser(..),DeleteUser(..),CreateAdmin(..),CreateAuthor(..),GetAuthor(..),UpdateAuthor(..),DeleteAuthor(..),CreateCategory(..),CreateSubCategory(..))

import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.List (delete,elemIndex)
import Data.Text (Text, unpack,append)
import Network.HTTP.Types.URI (queryToQueryText,QueryText)
import Network.Wai (Request (..))
import Oops (ReqError (..))
import TryRead (tryReadId, tryReadPage,tryReadSortOrd,tryReadDay,tryReadIdArray)
import Types
import Methods.Common.Exist (Handle, CheckExist(..),UncheckedExId(..))
import Data.Time.Calendar ( Day)
import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadCatch)

checkQStr :: (MonadCatch m,ParseQueryStr a,CheckExist a) => Handle m -> QueryText -> ExceptT ReqError m a
checkQStr h qStr = do
  a <- parseQueryStr qStr
  checkExist h a
  return a

checkEndPoint h meth path =
  resourses <- parsePath path
  checkExist h resourses
  toEP (meth,resourses)

data Methods =
  Post
  | GetAll 
  | Get Id
  | Put Id
  | Delete Id

data EndPoint =
  AdminEP
  | LogInEP
  | AuthorEP Methods
  | CatEP Methods
  | CommentEP Methods
  | DraftEP Methods
  | PictureEP Methods
  | PostEP Methods
  | TagEP Methods
  | UserEP Methods

toEP (meth,resourses) = case (meth,resourses) of
  (POST   ,[AdminR])               -> AdminEP 
  (POST   ,[LogInR])               -> LogInEP
  (POST   ,[AuthorR All])          -> AuthorEP Post
  (GET    ,[AuthorR (IdR iD)])     -> AuthorEP (Get iD)
  (PUT    ,[AuthorR (IdR iD)])     -> AuthorEP (Put iD)
  (DELETE ,[AuthorR (IdR iD)])     -> AuthorEP (Delete iD)
  (POST   ,[CatR All])             -> CatEP Post
  (GET    ,[CatR (IdR iD)])        -> CatEP (Get iD)
  (PUT    ,[CatR (IdR iD)])        -> CatEP (Put iD)
  (DELETE ,[CatR (IdR iD)])        -> CatEP (Delete iD)
  (POST   ,[CommentR All])         -> CommentEP Post
  (GET    ,[CommentR (IdR iD)])    -> CommentEP (Get iD)
  (PUT    ,[CommentR (IdR iD)])    -> CommentEP (Put iD)
  (DELETE ,[CommentR (IdR iD)])    -> CommentEP (Delete iD)
  (POST   ,[DraftR All])           -> DraftEP Post
  (GET    ,[DraftR (IdR iD)])      -> DraftEP (Get iD)
  (PUT    ,[DraftR (IdR iD)])      -> DraftEP (Put iD)
  (DELETE ,[DraftR (IdR iD)])      -> DraftEP (Delete iD)
  (POST   ,[PictureR All])         -> PictureEP Post
  (GET    ,[PictureR (IdR iD)])    -> PictureEP (Get iD)
  (PUT    ,[PictureR (IdR iD)])    -> PictureEP (Put iD)
  (DELETE ,[PictureR (IdR iD)])    -> PictureEP (Delete iD)
  (POST   ,[PostR All])            -> PostEP Post
  (GET    ,[PostR (IdR iD)])       -> PostEP (Get iD)
  (PUT    ,[PostR (IdR iD)])       -> PostEP (Put iD)
  (DELETE ,[PostR (IdR iD)])       -> PostEP (Delete iD)
  (POST   ,[TagR All])             -> TagEP Post
  (GET    ,[TagR (IdR iD)])        -> TagEP (Get iD)
  (PUT    ,[TagR (IdR iD)])        -> TagEP (Put iD)
  (DELETE ,[TagR (IdR iD)])        -> TagEP (Delete iD)
  (POST   ,[UserR All])            -> UserEP Post
  (GET    ,[UserR (IdR iD)])       -> UserEP (Get iD)
  (PUT    ,[UserR (IdR iD)])       -> UserEP (Put iD)
  (DELETE ,[UserR (IdR iD)])       -> UserEP (Delete iD)
  (x,y)                            -> throwE $ ResourseNotExistError $ "Unknown method-path combination: " ++ show (x,y)

data ResourseEntity =
  IdR Id
  | All

data Resourse = 
  AdminR
  | LogInR
  | AuthorR  ResourseEntity
  | CatR     ResourseEntity
  | CommentR ResourseEntity
  | DraftR   ResourseEntity
  | PictureR ResourseEntity
  | PostR    ResourseEntity
  | TagR     ResourseEntity
  | UserR    ResourseEntity

instance CheckExist Resourse where
  checkExist h (AuthorR  (IdR iD)) = isExistResourseE h (AuthorId   iD)
  checkExist h (CatR     (IdR iD)) = isExistResourseE h (CategoryId iD)
  checkExist h (CommentR (IdR iD)) = isExistResourseE h (CommentId  iD)
  checkExist h (DraftR   (IdR iD)) = isExistResourseE h (DraftId    iD)
  checkExist h (PictureR (IdR iD)) = isExistResourseE h (PictureId  iD)
  checkExist h (PostR    (IdR iD)) = isExistResourseE h (PostId     iD)
  checkExist h (TagR     (IdR iD)) = isExistResourseE h (TagId      iD)
  checkExist h (UserR    (IdR iD)) = isExistResourseE h (UserId     iD)
  checkExist _ _ = return ()

instance CheckExist [Resourse] where
  checkExist h xs = mapM_ (checkExist h) xs

class (Show a) => ParsePath a where
  parsePath :: (Monad m) => [Text] -> ExceptT ReqError m [Resourse]

toResourse :: (Monad m) => Text -> ExceptT ReqError m Resourse
toResourse "admins"     = return $ AdminR
toResourse "logIn"      = return $ LogInR
toResourse "authors"    = return $ AuthorR All
toResourse "categories" = return $ CatR All
toResourse "comments"   = return $ CommentR All
toResourse "drafts"     = return $ DraftR All
toResourse "pictures"   = return $ PictureR All
toResourse "posts"      = return $ PostR All
toResourse "tags"       = return $ TagR All
toResourse "users"      = return $ UserR All
toResourse txt          = throwE $ ResourseNotExistError $ getTxtstart txt

toResourseId :: (Monad m) => Text -> ExceptT ReqError m Resourse
toResourseId "authors"    idTxt = AuthorR  . IdR <$> parseResourseId "author"
toResourseId "categories" idTxt = CatR     . IdR <$> parseResourseId "category"
toResourseId "comments"   idTxt = CommentR . IdR <$> parseResourseId "comment"
toResourseId "drafts"     idTxt = DraftR   . IdR <$> parseResourseId "draft"
toResourseId "drafts"     idTxt = DraftR   . IdR <$> parseResourseId "draft"
toResourseId "pictures"   idTxt = PictureR . IdR <$> parseResourseId "pic"
toResourseId "posts"      idTxt = PostR    . IdR <$> parseResourseId "post"
toResourseId "tags"       idTxt = TagR     . IdR <$> parseResourseId "tag"
toResourseId "users"      idTxt = UserR    . IdR <$> parseResourseId "user"
toResourseId txt          idTxt = throwE $ ResourseNotExistError $ getTxtstart txt


parsePath :: (Monad m) => [Text] -> ExceptT ReqError m [Resourse]
parsePath [x] = [toResourse x]
parsePath [x,y] = [toResourseId x y]
parsePath x:y:xs = parsePath [x,y] ++ parsePath xs

parseResourseId :: (MonadCatch m) => Handle m -> ResourseName -> ResourseIdText -> ExceptT ReqError m Id
parseResourseId Handle{..} resName resIdTxt = do
  tryReadResourseId (append resName "_id") resIdTxt



checkResourseId :: (MonadCatch m) => Handle m -> ResourseId -> ExceptT ReqError m AuthorId
checkResourseId Handle{..} auIdTxt = do
  iD <- tryReadResourseId "author_id" auIdTxt
  isExistResourseE hExist (AuthorId iD)
  return iD

checkResourseId :: (MonadCatch m) => Handle m -> ResourseId -> ExceptT ReqError m AuthorId
checkResourseId Handle{..} auIdTxt = do
  iD <- tryReadResourseId "author_id" auIdTxt
  isExistResourseE hExist (AuthorId iD)
  return iD

data LogIn = LogIn {user_idLI :: UserId, passwordLI :: Text}
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


data CreateAdmin = CreateAdmin {keyCAK :: Text, pwdCAK :: Text, fNameCAK :: Text, lNameCAK :: Text, picIdCAK :: PictureId}
  deriving (Show)

instance ParseQueryStr CreateAdmin where
  parseQueryStr qStr =
    CreateAdmin
      <$> parseTxtParam qStr 50 "create_admin_key"
      <*> parseTxtParam qStr 50 "password"
      <*> parseTxtParam qStr 50 "first_name"
      <*> parseTxtParam qStr 50 "last_name"
      <*> parseIdParam qStr "user_pic_id"

instance CheckExist CreateAdmin where
  checkExist h (CreateAdmin _ _ _ _ picId) =
    checkExist h (PictureId picId)

data CreateAuthor = CreateAuthor {user_idCA :: UserId, author_infoCA :: Text}
  deriving (Show)

instance ParseQueryStr CreateAuthor where
  parseQueryStr qStr =
    CreateAuthor
      <$> parseIdParam qStr "user_id"
      <*> parseTxtParam qStr 500 "author_info"

instance CheckExist CreateAuthor where
  checkExist h (CreateAuthor usId _) =
    checkExist h (UserId usId)


data UpdateAuthor = UpdateAuthor { user_idUA :: Id, author_infoUA :: Text}
  deriving (Show)

instance ParseQueryStr UpdateAuthor where
  parseQueryStr qStr =
    UpdateAuthor
      <$> parseIdParam qStr "user_id"
      <*> parseTxtParam qStr 500 "author_info"

instance CheckExist UpdateAuthor where
  checkExist h (UpdateAuthor usId _) =
    checkExist h (UserId usId)

data CreateCategory = 
  CreateCategory Text (Maybe Id)
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

data UpdateTag = UpdateTag Text
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

data GetPosts = 
  GetPosts Page GetPostsF GetPostsOrd
  deriving (Show)
   
instance ParseQueryStr GetPosts where
  parseQueryStr qStr =
    GetPosts
      <$> parsePageParam qStr "page"
      <*> parseQueryStr qStr
      <*> parseQueryStr qStr

instance CheckExist GetPosts where
  checkExist h (GetPosts _ gpF _) = do
    checkExist h gpF


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
  parseQueryStr qStr =
    GetPostsF
      <$> parseMaybeDayParam      qStr "created_at"
      <*> parseMaybeDayParam      qStr "created_at_lt"
      <*> parseMaybeDayParam      qStr "created_at_gt"
      <*> parseMaybeIdParam       qStr "tag"
      <*> parseMaybeIdArrayParam  qStr "tags_in"
      <*> parseMaybeIdArrayParam  qStr "tags_all"
      <*> parseMaybeTxtParam   qStr 50 "name_in"
      <*> parseMaybeTxtParam   qStr 50 "text_in"
      <*> parseMaybeTxtParam   qStr 50 "everywhere_in"
      <*> parseMaybeTxtParam   qStr 50 "author_name"
      <*> parseMaybeIdParam       qStr "category_id"

instance CheckExist GetPostsF where
  checkExist h (GetPostsF _ _ _ maybTag maybTagsIn maybTagsAll _ _ _ _ maybCat) = do
    checkExist h $ fmap TagId maybTag
    checkExist h $ (fmap . fmap) TagId maybTagsIn
    checkExist h $ (fmap . fmap) TagId maybTagsAll
    checkExist h $ fmap CategoryId maybTag

data GetPostsOrd = 
  GetPostsOrd 
    (Maybe (SortOrd,Int))
    (Maybe (SortOrd,Int))
    (Maybe (SortOrd,Int))
    (Maybe (SortOrd,Int))
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

data UpdateComment = UpdateComment Text
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
parseTxtParam qStr leng paramKey = do
  paramTxt <- checkParam qStr paramKey
  checkLength leng paramKey paramTxt

parseIdParam :: (Monad m) => QueryText -> QueryParamKey -> ExceptT ReqError m Id
parseIdParam qStr paramKey = do
  paramTxt <- checkParam qStr paramKey
  tryReadId paramKey paramTxt

parsePageParam :: (Monad m) => QueryText -> QueryParamKey -> ExceptT ReqError m Page
parsePageParam qStr paramKey = do
  paramTxt <- checkParam qStr paramKey
  tryReadPage paramTxt

parseMaybeTxtParam :: (Monad m) => QueryText -> Int -> QueryParamKey -> ExceptT ReqError m (Maybe Text)
parseMaybeTxtParam qStr leng paramKey = do
  maybeParamTxt <- checkMaybeParam qStr paramKey
  case maybeParamTxt of
    Just paramTxt -> do
      txt <- checkLength leng paramKey paramTxt
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

parseMaybeSortOrdParam :: (Monad m) => QueryText -> QueryParamKey -> ExceptT ReqError m (Maybe (SortOrd,Int))
parseMaybeSortOrdParam qStr paramKey = do
  maybeParamTxt <- checkMaybeParam qStr paramKey
  case maybeParamTxt of
    Just paramTxt -> do
      sOrd <- tryReadSortOrd paramKey paramTxt
      let index = findParamIndex qStr paramKey
      return $ fmap (sOrd,) index
    Nothing -> return Nothing

checkParam :: (Monad m) => QueryText -> QueryParamKey -> ExceptT ReqError m Text
checkParam qStr paramKey = case lookup paramKey $ qStr of
  Just (Just "") -> throwE $ BadReqError $ "Can't parse parameter:" ++ unpack paramKey ++ ". Empty input."
  Just (Just txt) -> checkSingleParam qStr paramKey txt
  Just Nothing -> throwE $ BadReqError $ "Can't parse parameter:" ++ unpack paramKey
  Nothing -> throwE $ BadReqError $ "Can't find parameter:" ++ unpack paramKey

checkMaybeParam :: (Monad m) => QueryText -> QueryParamKey -> ExceptT ReqError m (Maybe Text)
checkMaybeParam qStr paramKey = case lookup paramKey $ qStr of
  Just (Just "") -> throwE $ BadReqError $ "Can't parse parameter:" ++ unpack paramKey ++ ". Empty input."
  Just (Just txt) -> do
    txt1 <- checkSingleParam qStr paramKey txt
    return (Just txt1)
  Just Nothing -> throwE $ BadReqError $ "Can't parse parameter:" ++ unpack paramKey
  Nothing -> return Nothing

checkSingleParam :: (Monad m) => QueryText -> QueryParamKey -> Text -> ExceptT ReqError m Text
checkSingleParam qStr paramKey txt = case lookup paramKey . delete (paramKey, Just txt) $ qStr of
  Nothing -> return txt
  Just _ -> throwE $ BadReqError $ "Multiple parameter: " ++ unpack paramKey

checkLength :: (Monad m) => Int -> QueryParamKey -> Text -> ExceptT ReqError m Text
checkLength leng paramKey txt = case splitAt leng (unpack txt) of
  (_, []) -> return txt
  _ -> throwE $ BadReqError $ "Parameter: " ++ unpack paramKey ++ " too long. Maximum length should be: " ++ show leng

findParamIndex :: QueryText -> QueryParamKey -> Maybe Int
findParamIndex qStr paramKey = elemIndex paramKey . fmap fst  $ qStr