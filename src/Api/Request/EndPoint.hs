module Api.Request.EndPoint where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Text (Text, append)
import Error (ReqError (..))
import Network.HTTP.Types (StdMethod (..))
import TryRead (getTxtstart, tryReadResourceId)
import Types

parseEndPoint ::
  (MonadCatch m) =>
  StdMethod ->
  [Text] ->
  ExceptT ReqError m EndPoint
parseEndPoint stdMeth path = do
  resources <- parsePath path
  toEP (stdMeth, resources)

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

toEP :: (MonadCatch m) => (StdMethod, [Resource]) -> ExceptT ReqError m EndPoint
toEP (stdMeth, resources) = case (stdMeth, resources) of
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
  (GET, [PostIdR iD]) -> return $ PostEP (ToGet iD)
  (GET, [PostR]) -> return $ PostEP ToGetAll
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
  (x, y) ->
    throwE
      $ ResourceNotExistError
      $ "Unknown method-path combination: " ++ show (x, y)

data Resource
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

parsePath :: (MonadCatch m) => [Text] -> ExceptT ReqError m [Resource]
parsePath [x] = (: []) <$> toResource x
parsePath [x, y] = (: []) <$> toResourceId x y
parsePath [x, y, z] = (++) <$> parsePath [x, y] <*> parsePath [z]
parsePath _ = throwE $ ResourceNotExistError "Path too long"

toResource :: (MonadCatch m) => Text -> ExceptT ReqError m Resource
toResource "admins" = return AdminR
toResource "logIn" = return LogInR
toResource "authors" = return AuthorR
toResource "categories" = return CatR
toResource "comments" = return CommentR
toResource "drafts" = return DraftR
toResource "pictures" = return PictureR
toResource "posts" = return PostR
toResource "tags" = return TagR
toResource "users" = return UserR
toResource txt = throwE $ ResourceNotExistError $ getTxtstart txt


toResourceId :: (MonadCatch m) => Text -> Text -> ExceptT ReqError m Resource
toResourceId "authors" idTxt = AuthorIdR <$> parseResourceId "author" idTxt
toResourceId "categories" idTxt = CatIdR <$> parseResourceId "category" idTxt
toResourceId "comments" idTxt = CommentIdR <$> parseResourceId "comment" idTxt
toResourceId "drafts" idTxt = DraftIdR <$> parseResourceId "draft" idTxt
toResourceId "pictures" idTxt = PictureIdR <$> parseResourceId "pic" idTxt
toResourceId "posts" idTxt = PostIdR <$> parseResourceId "post" idTxt
toResourceId "tags" idTxt = TagIdR <$> parseResourceId "tag" idTxt
toResourceId "users" idTxt = UserIdR <$> parseResourceId "user" idTxt
toResourceId txt _ = throwE $ ResourceNotExistError $ getTxtstart txt

parseResourceId ::
  (MonadCatch m) =>
  ResourceName ->
  ResourceIdText ->
  ExceptT ReqError m Id
parseResourceId resName = tryReadResourceId (append resName "_id")

