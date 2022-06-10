{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Api.Request.EndPoint where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Text (Text, append)
import Network.HTTP.Types (StdMethod (..))
import Oops (ReqError (..))
import TryRead (getTxtstart, tryReadResourseId)
import Types

parseEndPoint :: (MonadCatch m) => StdMethod -> [Text] -> ExceptT ReqError m EndPoint
parseEndPoint stdMeth path = do
  resourses <- parsePath path
  toEP (stdMeth, resourses)

data AppMethod
  = ToPost
  | ToPostId Id
  | ToGetAll
  | ToGet Id
  | ToPut Id
  | ToDelete Id
  deriving (Eq, Show)

data EndPoint
  = AdminEP
  | LogInEP
  | AuthorEP AppMethod
  | CatEP AppMethod
  | CommentEP AppMethod
  | DraftEP AppMethod
  | PictureEP AppMethod
  | PostEP AppMethod
  | TagEP AppMethod
  | UserEP AppMethod

toEP :: (MonadCatch m) => (StdMethod, [Resourse]) -> ExceptT ReqError m EndPoint
toEP (stdMeth, resourses) = case (stdMeth, resourses) of
  (POST, [AdminR]) -> return AdminEP
  (POST, [LogInR]) -> return LogInEP
  (POST, [AuthorR]) -> return $ AuthorEP ToPost
  (GET, [AuthorIdR iD]) -> return $ AuthorEP (ToGet iD)
  (PUT, [AuthorIdR iD]) -> return $ AuthorEP (ToPut iD)
  (DELETE, [AuthorIdR iD]) -> return $ AuthorEP (ToDelete iD)
  (POST, [CatR]) -> return $ CatEP ToPost
  (GET, [CatIdR iD]) -> return $ CatEP (ToGet iD)
  (PUT, [CatIdR iD]) -> return $ CatEP (ToPut iD)
  (DELETE, [CatIdR iD]) -> return $ CatEP (ToDelete iD)
  (POST, [CommentR]) -> return $ CommentEP ToPost
  (GET, [CommentR]) -> return $ CommentEP ToGetAll
  (GET, [CommentIdR iD]) -> return $ CommentEP (ToGet iD)
  (PUT, [CommentIdR iD]) -> return $ CommentEP (ToPut iD)
  (DELETE, [CommentIdR iD]) -> return $ CommentEP (ToDelete iD)
  (POST, [DraftR]) -> return $ DraftEP ToPost
  (POST, [DraftIdR iD, PostR]) -> return $ DraftEP (ToPostId iD)
  (GET, [DraftIdR iD]) -> return $ DraftEP (ToGet iD)
  (GET, [DraftR]) -> return $ DraftEP ToGetAll
  (PUT, [DraftIdR iD]) -> return $ DraftEP (ToPut iD)
  (DELETE, [DraftIdR iD]) -> return $ DraftEP (ToDelete iD)
  (POST, [PictureR]) -> return $ PictureEP ToPost
  (GET, [PictureIdR iD]) -> return $ PictureEP (ToGet iD)
  (POST, [PostIdR iD, DraftR]) -> return $ PostEP (ToPostId iD)
  (GET, [PostIdR iD]) -> return $ PostEP (ToGet iD)
  (PUT, [PostIdR iD]) -> return $ PostEP (ToPut iD)
  (DELETE, [PostIdR iD]) -> return $ PostEP (ToDelete iD)
  (POST, [TagR]) -> return $ TagEP ToPost
  (GET, [TagIdR iD]) -> return $ TagEP (ToGet iD)
  (PUT, [TagIdR iD]) -> return $ TagEP (ToPut iD)
  (DELETE, [TagIdR iD]) -> return $ TagEP (ToDelete iD)
  (POST, [UserR]) -> return $ UserEP ToPost
  (GET, [UserIdR iD]) -> return $ UserEP (ToGet iD)
  (PUT, [UserIdR iD]) -> return $ UserEP (ToPut iD)
  (DELETE, [UserIdR iD]) -> return $ UserEP (ToDelete iD)
  (x, y) -> throwE $ ResourseNotExistError $ "Unknown method-path combination: " ++ show (x, y)

data Resourse
  = AdminR
  | LogInR
  | AuthorR
  | CatR
  | CommentR
  | DraftR
  | PictureR
  | PostR
  | TagR
  | UserR
  | AuthorIdR Id
  | CatIdR Id
  | CommentIdR Id
  | DraftIdR Id
  | PictureIdR Id
  | PostIdR Id
  | TagIdR Id
  | UserIdR Id
  deriving (Eq, Show)

parsePath :: (MonadCatch m) => [Text] -> ExceptT ReqError m [Resourse]
parsePath [x] = (: []) <$> toResourse x
parsePath [x, y] = (: []) <$> toResourseId x y
parsePath [x, y, z] = (++) <$> parsePath [x, y] <*> parsePath [z]
parsePath _ = throwE $ ResourseNotExistError "Path too long"

toResourse :: (MonadCatch m) => Text -> ExceptT ReqError m Resourse
toResourse "admins" = return AdminR
toResourse "logIn" = return LogInR
toResourse "authors" = return AuthorR
toResourse "categories" = return CatR
toResourse "comments" = return CommentR
toResourse "drafts" = return DraftR
toResourse "pictures" = return PictureR
toResourse "posts" = return PostR
toResourse "tags" = return TagR
toResourse "users" = return UserR
toResourse txt = throwE $ ResourseNotExistError $ getTxtstart txt

toResourseId :: (MonadCatch m) => Text -> Text -> ExceptT ReqError m Resourse
toResourseId "authors" idTxt = AuthorIdR <$> parseResourseId "author" idTxt
toResourseId "categories" idTxt = CatIdR <$> parseResourseId "category" idTxt
toResourseId "comments" idTxt = CommentIdR <$> parseResourseId "comment" idTxt
toResourseId "drafts" idTxt = DraftIdR <$> parseResourseId "draft" idTxt
toResourseId "pictures" idTxt = PictureIdR <$> parseResourseId "pic" idTxt
toResourseId "posts" idTxt = PostIdR <$> parseResourseId "post" idTxt
toResourseId "tags" idTxt = TagIdR <$> parseResourseId "tag" idTxt
toResourseId "users" idTxt = UserIdR <$> parseResourseId "user" idTxt
toResourseId txt _ = throwE $ ResourseNotExistError $ getTxtstart txt

parseResourseId :: (MonadCatch m) => ResourseName -> ResourseIdText -> ExceptT ReqError m Id
parseResourseId resName = tryReadResourseId (append resName "_id")


