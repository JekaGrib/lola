{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Network.HTTP.Simple            ( parseRequest, setRequestBody, getResponseBody, httpLBS )
import           Network.Wai
import           Network.HTTP.Types             ( status200, status404, status301, movedPermanently301 )
import           Network.HTTP.Types.URI         ( queryToQueryText )
import           Network.Wai.Handler.Warp       ( run )
import           Data.Aeson
import           Data.Text                      ( pack, unpack, Text, concat, toUpper, stripPrefix, isPrefixOf )
import           Data.ByteString.Builder        ( lazyByteString )
import           Database.PostgreSQL.Simple
import           Network.HTTP.Simple            ( parseRequest, setRequestBody, getResponseBody, httpLBS, HttpException )
import           Data.Maybe                     ( fromJust )
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Calendar             ( showGregorian, Day )
import           Database.PostgreSQL.Simple.Time
import           Data.String                    ( fromString )
import           Data.List                      ( intercalate, zip4 )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )
import           Codec.Picture                  ( decodeImage )
import           Data.ByteString.Lazy           ( toStrict )
import           Control.Monad.Catch            ( catch, throwM )

defaultPictureUrl :: Text
defaultPictureUrl = "https://cdn.pixabay.com/photo/2020/01/14/09/20/anonym-4764566_960_720.jpg"
defUsId = 1
defPicId = 1
defAuthId = 1
defCatId = 1

commentNumberLimit = 20
draftNumberLimit = 5
postNumberLimit = 5


getDay :: IO String
getDay = do
  time    <- getZonedTime
  let day = showGregorian . localDay . zonedTimeToLocalTime $ time
  return day



getTime :: IO String
getTime = do
  time    <- getZonedTime
  return $ show time     
   

createAuthorsTable :: IO ()
createAuthorsTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE authors ( author_id BIGSERIAL PRIMARY KEY NOT NULL, author_info VARCHAR(1000) NOT NULL, user_id BIGINT NOT NULL REFERENCES users(user_id), UNIQUE (user_id) )"
  print "kk"

createPicsTable :: IO ()
createPicsTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE pics ( pic_id BIGSERIAL PRIMARY KEY NOT NULL, pic_url VARCHAR(1000) NOT NULL)"
  print "kk"

createDefaultPicture :: IO Integer
createDefaultPicture = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  [Only picId] <- query conn "INSERT INTO pics ( pic_url ) VALUES (?) RETURNING pic_id " [defaultPictureUrl]
  return picId

createDefaultUser picId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  day <- getDay  
  [Only userId] <- query conn "INSERT INTO users ( password, first_name , last_name , user_pic_id , user_create_date, admin) VALUES ( '12345678','DELETED','DELETED',?,?, false ) RETURNING user_id" [ pack (show picId), pack day ]
  return userId

createDefaultAuthor userId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  [Only authorId] <- query conn "INSERT INTO authors ( user_id , author_info) VALUES ( ?,'DELETED' ) RETURNING author_id" [pack . show $ userId]  
  return authorId

createDefaultCategory :: IO Integer
createDefaultCategory = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  [Only catId] <- query_ conn "INSERT INTO categories (category_name) VALUES ( 'NONE' ) RETURNING category_id" 
  return catId

createCreateAdminKeyTable :: IO ()
createCreateAdminKeyTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE key ( create_admin_key VARCHAR(100) NOT NULL)"
  print "kk"

createUsersTable :: IO ()
createUsersTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE users ( user_id BIGSERIAL PRIMARY KEY NOT NULL, password VARCHAR(50) NOT NULL, first_name VARCHAR(50) NOT NULL, last_name  VARCHAR(50) NOT NULL, user_pic_id BIGINT NOT NULL REFERENCES pics(pic_id), user_create_date DATE NOT NULL, admin boolean NOT NULL)"
  print "kk"

createTagsTable :: IO ()
createTagsTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE tags ( tag_id BIGSERIAL PRIMARY KEY NOT NULL, tag_name VARCHAR(50) NOT NULL)"
  print "kk"

createCategoriesTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE categories ( category_id BIGSERIAL PRIMARY KEY NOT NULL, category_name VARCHAR(50) NOT NULL, super_category_id BIGINT)"
  print "kk"

createPostsTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE posts ( post_id BIGSERIAL PRIMARY KEY NOT NULL, author_id BIGINT REFERENCES authors(author_id), post_name VARCHAR(100) NOT NULL, post_create_date DATE NOT NULL, post_category_id BIGINT NOT NULL REFERENCES categories(category_id), post_text VARCHAR(10000) NOT NULL, post_main_pic_id BIGINT NOT NULL REFERENCES pics(pic_id))"
  print "kk"

createCommentsTable :: IO ()
createCommentsTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE comments ( comment_id BIGSERIAL PRIMARY KEY NOT NULL, comment_text VARCHAR(1000) NOT NULL, post_id BIGINT NOT NULL REFERENCES posts(post_id), user_id BIGINT NOT NULL REFERENCES users(user_id))"
  print "kk"

createPostsPicsTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE postspics ( post_id BIGINT NOT NULL REFERENCES posts(post_id), pic_id BIGINT NOT NULL REFERENCES pics(pic_id))"
  print "kk"

createPostsTagsTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE poststags ( post_id BIGINT NOT NULL REFERENCES posts(post_id), tag_id BIGINT NOT NULL REFERENCES tags(tag_id))"
  print "kk"

createDraftsTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE drafts ( draft_id BIGSERIAL PRIMARY KEY NOT NULL, post_id BIGINT REFERENCES posts(post_id), author_id BIGINT REFERENCES authors(author_id), draft_name VARCHAR(100) NOT NULL, draft_category_id BIGINT NOT NULL REFERENCES categories(category_id), draft_text VARCHAR(10000) NOT NULL, draft_main_pic_id BIGINT NOT NULL REFERENCES pics(pic_id))"
  print "kk"

createDraftsPicsTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE draftspics ( draft_id BIGINT NOT NULL REFERENCES drafts(draft_id), pic_id BIGINT NOT NULL REFERENCES pics(pic_id))"
  print "kk"

createDraftsTagsTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE draftstags ( draft_id BIGINT NOT NULL REFERENCES drafts(draft_id), tag_id BIGINT NOT NULL REFERENCES tags(tag_id))"
  print "kk"

addCreateAdminKey = do
  conn1 <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn1 "INSERT INTO key (create_admin_key) VALUES ( 'lola' ) "

createDbStructure = do
  createPicsTable
  createCreateAdminKeyTable
  createUsersTable
  createAuthorsTable
  createTagsTable
  createCategoriesTable
  createPostsTable
  createCommentsTable
  createPostsPicsTable
  createPostsTagsTable
  createDraftsTable
  createDraftsPicsTable
  createDraftsTagsTable

addDefaultParameters = do
  addCreateAdminKey
  picId <- createDefaultPicture
  userId <- createDefaultUser picId
  authorId <- createDefaultAuthor userId
  createDefaultCategory
  return [picId,userId,authorId]
  



data UserResponse = UserResponse {
      user_id      :: Integer
    , first_name   :: Text
    , last_name    :: Text
    , user_pic_id  :: Integer
    , user_pic_url :: Text
    , user_create_date :: Text
    } deriving Show

instance ToJSON UserResponse where
    toJSON (UserResponse user_id first_name last_name user_pic_id user_pic_url user_create_date) =
        object ["user_id" .= user_id, "first_name" .= first_name, "last_name" .= last_name, "user_pic_id" .= user_pic_id, "user_pic_url" .= user_pic_url, "user_create_date" .= user_create_date]
    toEncoding (UserResponse user_id first_name last_name user_pic_id user_pic_url user_create_date) =
        pairs ("user_id" .= user_id <> "first_name" .= first_name <> "last_name" .= last_name <> "user_pic_id" .= user_pic_id <> "user_pic_url" .= user_pic_url <> "user_create_date" .= user_create_date)

data OkResponse = OkResponse {ok :: Bool}

instance ToJSON OkResponse where
    toJSON (OkResponse ok) =
        object ["ok" .= ok]
    toEncoding (OkResponse ok) =
        pairs ("ok" .= ok )

data OkInfoResponse = OkInfoResponse {ok7 :: Bool, info7 :: Text}

instance ToJSON OkInfoResponse where
    toJSON (OkInfoResponse ok info) =
        object ["ok" .= ok, "info" .= info]
    toEncoding (OkInfoResponse ok info) =
        pairs ("ok" .= ok <> "info" .= info )



data AuthorResponse = AuthorResponse {
      author_id    :: Integer
    , auth_user_id :: Integer
    , author_info  :: Text
    } deriving Show

instance ToJSON AuthorResponse where
    toJSON (AuthorResponse author_id auth_user_id author_info ) =
        object ["author_id" .= author_id, "user_id" .= auth_user_id, "author_info" .= author_info]
    toEncoding (AuthorResponse author_id auth_user_id author_info ) =
        pairs ( "author_id" .= author_id <> "user_id" .= auth_user_id <> "author_info" .= author_info)

data CatResponse 
  = SubCatResponse {
      subCat_id      :: Integer
    , subCat_name    :: Text
    , super_category :: CatResponse
    } 
  | CatResponse {
      cat_id    :: Integer
    , cat_name  :: Text
    , super_cat :: Text
    } deriving Show


instance ToJSON CatResponse where
    toJSON (CatResponse cat_id cat_name super_cat) =
        object ["category_id" .= cat_id, "category_name" .= cat_name, "super_category" .= super_cat]
    toJSON (SubCatResponse cat_id cat_name super_cat) =
        object ["category_id" .= cat_id, "category_name" .= cat_name, "super_category" .= super_cat]
    toEncoding (CatResponse cat_id cat_name super_cat) =
        pairs ( "category_id" .= cat_id <> "category_name" .= cat_name <> "super_category" .= super_cat)
    toEncoding (SubCatResponse cat_id cat_name super_cat) =
        pairs ( "category_id" .= cat_id <> "category_name" .= cat_name <> "super_category" .= super_cat)

data DraftRequest = DraftRequest {
      user_id1      :: Integer
    , password1   :: Text
    , draft_name    :: Text
    , draft_cat_id :: Integer
    , draft_text1  :: Text
    , draft_main_pic_url :: Text
    , draft_pics_urls :: [PicUrl]
    , draft_tags_ids :: [TagId]
    } deriving Show

instance FromJSON DraftRequest where
    parseJSON (Object v) = DraftRequest
        <$> v .: "user_id"
        <*> v .: "password"
        <*> v .: "draft_name"
        <*> v .: "draft_category_id"
        <*> v .: "draft_text"
        <*> v .: "draft_main_pic_url"
        <*> v .: "draft_pics_urls"
        <*> v .: "draft_tags_ids" 

instance ToJSON DraftRequest where
    toJSON (DraftRequest user_id1 password1 draft_name draft_cat_id draft_text1 draft_main_pic_url draft_pics_urls draft_tags_ids ) =
        object ["user_id" .= user_id1, "password" .= password1, "draft_name" .= draft_name, "draft_category_id" .= draft_cat_id, "draft_text" .= draft_text1, "draft_main_pic_url" .= draft_main_pic_url, "draft_pics_urls" .= draft_pics_urls, "draft_tags_ids" .= draft_tags_ids]
    toEncoding (DraftRequest user_id1 password1 draft_name draft_cat_id draft_text1 draft_main_pic_url draft_pics_urls draft_tags_ids) =
        pairs ("user_id" .= user_id1 <> "password" .= password1 <> "draft_name" .= draft_name <> "draft_category_id" .= draft_cat_id <> "draft_text" .= draft_text1 <> "draft_main_pic_url" .= draft_main_pic_url <> "draft_pics_urls" .= draft_pics_urls <> "draft_tags_ids" .= draft_tags_ids)

data PicUrl = PicUrl {
      pic_url :: Text
    } deriving Show

instance FromJSON PicUrl where
    parseJSON (Object v) = PicUrl
        <$> v .: "pic_url"

instance ToJSON PicUrl where
    toJSON (PicUrl pic_url ) =
        object ["pic_url" .= pic_url]
    toEncoding (PicUrl pic_url ) =
        pairs ( "pic_url" .= pic_url )

data TagId = TagId {
      tag_id3 :: Integer
    } deriving Show

instance ToJSON TagId where
    toJSON (TagId tag_id3 ) =
        object ["tag_id" .= tag_id3]
    toEncoding (TagId tag_id3 ) =
        pairs ( "tag_id" .= tag_id3 )

instance FromJSON TagId where
    parseJSON (Object v) = TagId
        <$> v .: "tag_id"


data Post = PostInteger Integer | PostText Text

instance Show Post where
  show (PostInteger a) = show a
  show (PostText a) = unpack a

instance ToJSON Post where
  toJSON (PostInteger a) = toJSON a
  toJSON (PostText a) = toJSON a
  

data DraftResponse = DraftResponse {
      draft_id2      :: Integer
    , post_id2      :: Post
    , author2   :: AuthorResponse
    , draft_name2    :: Text
    , draft_cat2 :: CatResponse
    , draft_text2  :: Text
    , draft_main_pic_id2  :: Integer
    , draft_main_pic_url2 :: Text
    , draft_pics2 :: [PicIdUrl]
    , draft_tags2 ::  [TagResponse]
    } deriving Show

instance ToJSON DraftResponse where
    toJSON (DraftResponse draft_id post_id author draft_name draft_cat draft_text draft_main_pic_id draft_main_pic_url draft_pics draft_tags) =
        object ["draft_id" .= draft_id, "post_id" .= post_id, "author" .= author, "draft_name" .= draft_name, "draft_category" .= draft_cat, "draft_text" .= draft_text, "draft_main_pic_id" .= draft_main_pic_id, "draft_main_pic_url" .= draft_main_pic_url, "draft_pics" .= draft_pics, "draft_tags" .= draft_tags]
    toEncoding (DraftResponse draft_id post_id author draft_name draft_cat draft_text draft_main_pic_id draft_main_pic_url draft_pics draft_tags ) =
        pairs ("draft_id" .= draft_id <> "post_id" .= post_id <> "author" .= author <> "draft_name" .= draft_name <> "draft_category" .= draft_cat <> "draft_text" .= draft_text <> "draft_main_pic_id" .= draft_main_pic_id <> "draft_main_pic_url" .= draft_main_pic_url <> "draft_pics" .= draft_pics <> "draft_tags" .= draft_tags)


data PicIdUrl = PicIdUrl {
      pic_id   :: Integer
    , pic_url2 :: Text
    } deriving Show

instance ToJSON PicIdUrl where
    toJSON (PicIdUrl pic_id2 pic_url ) =
        object ["pic_id" .= pic_id2, "pic_url" .= pic_url]
    toEncoding (PicIdUrl pic_id2 pic_url ) =
        pairs ( "pic_id" .= pic_id2 <> "pic_url" .= pic_url )

data DraftsResponse = DraftsResponse {
      page9     :: Integer
    , drafts9 :: [DraftResponse]
    } deriving Show

instance ToJSON DraftsResponse where
    toJSON (DraftsResponse page drafts ) =
        object ["page" .= page, "drafts" .= drafts]
    toEncoding (DraftsResponse page drafts ) =
        pairs ( "page" .= page <> "drafts" .= drafts )
  

data PostResponse = PostResponse {
      post_id      :: Integer
    , author4   :: AuthorResponse
    , post_name    :: Text
    , post_create_date :: Text
    , post_cat     :: CatResponse
    , post_text    :: Text
    , post_main_pic_id  :: Integer
    , post_main_pic_url :: Text
    , post_pics :: [PicIdUrl]
    , post_tags :: [TagResponse]
    } deriving Show

instance ToJSON PostResponse where
    toJSON (PostResponse post_id author4 post_name post_create_date post_cat post_text post_main_pic_id post_main_pic_url post_pics post_tags) =
        object ["post_id" .= post_id, "author" .= author4, "post_name" .= post_name, "post_create_date" .= post_create_date, "post_category" .= post_cat, "post_text" .= post_text, "post_main_pic_id" .= post_main_pic_id, "post_main_pic_url" .= post_main_pic_url, "post_pics" .= post_pics, "post_tags" .= post_tags]
    toEncoding (PostResponse post_id author4 post_name post_create_date post_cat post_text post_main_pic_id post_main_pic_url post_pics post_tags) =
        pairs ("post_id" .= post_id <> "author" .= author4  <> "post_name" .= post_name <> "post_create_date" .= post_create_date <> "post_category" .= post_cat <> "post_text" .= post_text <> "post_main_pic_id" .= post_main_pic_id <> "post_main_pic_url" .= post_main_pic_url <> "post_pics" .= post_pics <> "post_tags" .= post_tags)

data PostsResponse = PostsResponse {
      page10     :: Integer
    , posts10 :: [PostResponse]
    } deriving Show

instance ToJSON PostsResponse where
    toJSON (PostsResponse page posts ) =
        object ["page" .= page, "posts" .= posts]
    toEncoding (PostsResponse page posts ) =
        pairs ( "page" .= page <> "posts" .= posts )

data TagResponse = TagResponse {
      tag_id   :: Integer
    , tag_name :: Text
    } deriving Show

instance ToJSON TagResponse where
    toJSON (TagResponse tag_id tag_name ) =
        object ["tag_id" .= tag_id, "tag_name" .= tag_name]
    toEncoding (TagResponse tag_id tag_name ) =
        pairs ( "tag_id" .= tag_id <> "tag_name" .= tag_name )

data CommentResponse = CommentResponse {
      comment_id   :: Integer
    , comment_text :: Text
    , post_id6   :: Integer
    , user_id6   :: Integer
    } deriving Show

instance ToJSON CommentResponse where
    toJSON (CommentResponse comment_id comment_text post_id user_id) =
        object ["comment_id" .= comment_id, "comment_text" .= comment_text, "post_id" .= post_id, "user_id" .= user_id]
    toEncoding (CommentResponse comment_id comment_text post_id user_id) =
        pairs ( "comment_id" .= comment_id <> "comment_text" .= comment_text <> "post_id" .= post_id <> "user_id" .= user_id )

data CommentIdTextUserResponse = CommentIdTextUserResponse {
      comment_id8   :: Integer
    , comment_text8 :: Text
    , user_id8   :: Integer
    } deriving Show

instance ToJSON CommentIdTextUserResponse where
    toJSON (CommentIdTextUserResponse comment_id comment_text user_id) =
        object ["comment_id" .= comment_id, "comment_text" .= comment_text, "user_id" .= user_id]
    toEncoding (CommentIdTextUserResponse comment_id comment_text user_id) =
        pairs ( "comment_id" .= comment_id <> "comment_text" .= comment_text <> "user_id" .= user_id )

data CommentsResponse = CommentsResponse {
      page     :: Integer
    , post_id9 :: Integer
    , comments :: [CommentIdTextUserResponse]
    } deriving Show

instance ToJSON CommentsResponse where
    toJSON (CommentsResponse page post_id comments) =
        object ["page" .= page, "post_id" .= post_id, "comments" .= comments]
    toEncoding (CommentsResponse page post_id comments) =
        pairs ( "page" .= page <> "post_id" .= post_id <> "comments" .= comments )


application req send = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  ansE <- runExceptT $ answerEx conn req
  send ( fromEx ansE )

fromEx :: Either ReqError Response -> Response
fromEx ansE = 
  let okHelper = responseBuilder status200 [("Content-Type", "application/json; charset=utf-8")] . lazyByteString in
  case ansE of
    Right a              -> a
    Left (SimpleError str) -> okHelper . encode $ OkInfoResponse {ok7 = False, info7 = pack str}
    Left SecretError      -> responseBuilder status404 [] $ "Status 404 Not Found"

answerEx :: Connection -> Request -> ExceptT ReqError IO Response
answerEx conn req = do
   let okHelper = responseBuilder status200 [("Content-Type", "application/json; charset=utf-8")] . lazyByteString 
   case pathInfo req of
    ["createUser"] -> do
      let paramsNames = ["password","first_name","last_name","user_pic_url"]
      [pwdParam,fNameParam,lNameParam,picUrlParam] <- mapM (checkParam req) paramsNames
      picId <- getPicId conn picUrlParam
      day <- lift getDay
      let insNames  = ["password","first_name","last_name","user_pic_id"    ,"user_create_date","admin"]
      let insValues = [pwdParam  ,fNameParam  ,lNameParam ,pack (show picId),pack day          ,"FALSE"]
      usId <- lift $ insertReturnInDb conn "users" insNames insValues "user_id"
      return . okHelper . encode $ UserResponse {user_id = usId, first_name = fNameParam, last_name = lNameParam, user_pic_id = picId, user_pic_url = voo picId, user_create_date = pack day}
    ["getUser", usId] -> do
      usIdNum <- tryRead usId
      let selectParams = ["first_name","last_name","user_pic_id","user_create_date"]
      isExistInDbE conn "users" "user_id=?" [usId] "user_id"
      (fName,lName,picId,usCreateDate) <- selectTupleFromDbE conn "users" "user_id=?" [usId] selectParams
      return . okHelper . encode $ UserResponse {user_id = usIdNum, first_name = fName, last_name = lName, user_pic_id = picId, user_pic_url = voo picId, user_create_date = pack . showGregorian $ usCreateDate}
    ["deleteUser"] -> do
      let paramsNames = ["user_id","admin_id","password"]
      [usIdParam,admIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [usIdNum,admIdNum]              <- mapM tryRead [usIdParam,admIdParam] :: ExceptT ReqError IO [Integer]
      isExistInDbE conn "users" "user_id=?" [admIdParam] "user_id"
      (pwd,admBool) <- selectTupleFromDbE conn "users" "user_id=?" [admIdParam] ["password","admin"]
      adminAuth pwdParam pwd admBool
      isExistInDbE conn "users" "user_id=?" [usIdParam] "user_id"
      lift $ updateInDb conn "comments" "user_id=?" "user_id=?" [pack . show $ defUsId,usIdParam]
      check <- lift $ isExistInDb conn "authors" "user_id=?" [usIdParam] "author_id"
      case check of
        True -> do
          authorId <- selectFromDbE conn "authors" "user_id=?" [usIdParam] "author_id" :: ExceptT ReqError IO Integer
          lift $ updateInDb conn "posts" "author_id=?" "author_id=?" [pack . show $ defAuthId,pack . show $ authorId]
          draftsIds <- selectListFromDbE conn "drafts" "author_id=?" [pack . show $ authorId] "draft_id" :: ExceptT ReqError IO [Integer]
          deleteAllAboutDrafts conn draftsIds
          lift $ deleteFromDb conn "authors" "author_id=?" [pack . show $ authorId] 
          lift $ deleteFromDb conn "users" "user_id=?" [usIdParam]
          return . okHelper . encode $ OkResponse {ok = True} 
        False -> do
          lift $ deleteFromDb conn "users" "user_id=?" [usIdParam]
          return . okHelper . encode $ OkResponse {ok = True} 
    ["createAdmin"]        -> do
      let paramsNames = ["create_admin_key","password","first_name","last_name","user_pic_url"]
      [keyParam,pwdParam,fNameParam,lNameParam,picUrlParam] <- mapM (checkParam req) paramsNames
      key <- selectFromDbE conn "key" "true" ([]::[Text]) "create_admin_key" 
      checkKeyE keyParam key
      picId <- getPicId conn picUrlParam 
      day <- lift getDay
      let insNames  = ["password","first_name","last_name","user_pic_id"    ,"user_create_date","admin"]
      let insValues = [pwdParam  ,fNameParam  ,lNameParam ,pack (show picId),pack day          ,"TRUE" ]
      admId <- lift $ insertReturnInDb conn "users" insNames insValues "user_id"  
      return . okHelper . encode $ UserResponse {user_id = admId, first_name = fNameParam, last_name = lNameParam, user_pic_id = picId, user_pic_url = voo picId, user_create_date = pack day }
    ["createAuthor"]        -> do
      let paramsNames = ["user_id","author_info","admin_id","password"]
      [usIdParam,auInfoParam,admIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [usIdNum,admIdNum]                          <- mapM tryRead [usIdParam,admIdParam] :: ExceptT ReqError IO [Integer]
      isExistInDbE conn "users" "user_id=?" [admIdParam] "user_id"
      (pwd,admBool) <- selectTupleFromDbE conn "users" "user_id=?" [admIdParam] ["password","admin"]
      adminAuth pwdParam pwd admBool
      isExistInDbE      conn "users"   "user_id=?" [usIdParam] "user_id"
      ifExistInDbThrowE conn "authors" "user_id=?" [usIdParam] "user_id"
      auId <- lift $ insertReturnInDb conn "authors" ["user_id","author_info"] [usIdParam,auInfoParam] "user_id"
      return . okHelper . encode $ AuthorResponse {author_id = auId, auth_user_id = usIdNum, author_info = auInfoParam}
    ["getAuthor"]        -> do
      let paramsNames = ["author_id","admin_id","password"]
      [auIdParam,admIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [auIdNum,admIdNum]              <- mapM tryRead [auIdParam,admIdParam] :: ExceptT ReqError IO [Integer]
      isExistInDbE conn "users" "user_id=?" [admIdParam] "user_id"
      (pwd,admBool) <- selectTupleFromDbE conn "users" "user_id=?" [admIdParam] ["password","admin"]
      adminAuth pwdParam pwd admBool
      isExistInDbE conn "authors" "author_id=?" [auIdParam] "author_id"
      (usId,auInfo) <- selectTupleFromDbE conn "authors" "author_id=?" [auIdParam] ["user_id","author_info"]
      return . okHelper . encode $ AuthorResponse {author_id = auIdNum, auth_user_id = usId, author_info = auInfo}
    ["updateAuthor"]        -> do
      let paramsNames = ["author_id","user_id","author_info","admin_id","password"]
      [auIdParam,usIdParam,auInfoParam,admIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [auIdNum,usIdNum,admIdNum]                            <- mapM tryRead [auIdParam,usIdParam,admIdParam] :: ExceptT ReqError IO [Integer]
      isExistInDbE conn "users" "user_id=?" [admIdParam] "user_id"
      (pwd,admBool) <- selectTupleFromDbE conn "users" "user_id=?" [admIdParam] ["password","admin"]
      adminAuth pwdParam pwd admBool
      isExistInDbE conn "users" "user_id=?" [usIdParam] "user_id"
      isExistInDbE conn "authors" "author_id=?" [auIdParam] "author_id"
      checkRelationUsAu conn usIdParam auIdParam
      lift $ updateInDb conn "authors" "author_info=?" "author_id=?" [auInfoParam,auIdParam]
      return . okHelper . encode $ AuthorResponse {author_id = auIdNum, auth_user_id = usIdNum, author_info = auInfoParam}
    ["deleteAuthor"]   -> do
      let paramsNames = ["author_id","admin_id","password"]
      [auIdParam,admIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [auIdNum,admIdNum]              <- mapM tryRead [auIdParam,admIdParam] :: ExceptT ReqError IO [Integer]
      isExistInDbE conn "users" "user_id=?" [admIdParam] "user_id"
      (pwd,admBool) <- selectTupleFromDbE conn "users" "user_id=?" [admIdParam] ["password","admin"]
      adminAuth pwdParam pwd admBool
      isExistInDbE conn "authors" "author_id=?" [auIdParam] "author_id"
      lift $ updateInDb conn "posts" "author_id=?" "author_id=?" [pack . show $ defAuthId,auIdParam]
      draftsIds <- selectListFromDbE conn "drafts" "author_id=?" [auIdParam] "draft_id" :: ExceptT ReqError IO [Integer]
      deleteAllAboutDrafts conn draftsIds
      lift $ deleteFromDb conn "authors" "author_id=?" [auIdParam]
      return . okHelper . encode $ OkResponse {ok = True}
    ["createCategory"]        -> do
      let paramsNames = ["category_name","admin_id","password"]
      [catNameParam,admIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      admIdNum                           <- tryRead admIdParam :: ExceptT ReqError IO Integer
      isExistInDbE conn "users" "user_id=?" [admIdParam] "user_id"
      (pwd,admBool) <- selectTupleFromDbE conn "users" "user_id=?" [admIdParam] ["password","admin"]
      adminAuth pwdParam pwd admBool
      catId <- lift $ insertReturnInDb conn "categories" ["category_name"] [catNameParam] "category_id"
      return . okHelper . encode $ CatResponse {cat_id = catId, cat_name = catNameParam, super_cat = "NULL"}
    ["createSubCategory"]        -> do
      let paramsNames = ["category_name","super_category_id","admin_id","password"]
      [catNameParam,superCatIdParam,admIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [superCatIdNum,admIdNum]                           <- mapM tryRead [superCatIdParam,admIdParam] :: ExceptT ReqError IO [Integer]
      isExistInDbE conn "users" "user_id=?" [admIdParam] "user_id"
      (pwd,admBool) <- selectTupleFromDbE conn "users" "user_id=?" [admIdParam] ["password","admin"]
      adminAuth pwdParam pwd admBool
      isExistInDbE conn "categories" "category_id=?" [superCatIdParam] "category_id"
      catId <- lift $ insertReturnInDb conn "categories" ["category_name","super_category_id"] [catNameParam,superCatIdParam] "category_id"
      allSuperCats <- findAllSuperCats conn catId
      return . okHelper . encode $ inCatResp allSuperCats
    ["getCategory", catId] -> do
      catIdNum <- tryRead catId
      isExistInDbE conn "categories" "category_id=?" [catId] "category_id"
      allSuperCats <- findAllSuperCats conn catIdNum
      return . okHelper . encode $ inCatResp allSuperCats
    ["updateCategory"] -> do
      let paramsNames = ["category_id","category_name","super_category_id","admin_id","password"]
      [catIdParam,catNameParam,superCatIdParam,admIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [catIdNum,superCatIdNum,admIdNum]                             <- mapM tryRead [catIdParam,superCatIdParam,admIdParam] :: ExceptT ReqError IO [Integer]
      isExistInDbE conn "users" "user_id=?" [admIdParam] "user_id"
      (pwd,admBool) <- selectTupleFromDbE conn "users" "user_id=?" [admIdParam] ["password","admin"]
      adminAuth pwdParam pwd admBool
      isExistInDbE conn "categories" "category_id=?" [catIdParam]      "category_id"
      isExistInDbE conn "categories" "category_id=?" [superCatIdParam] "category_id"
      checkRelationCats conn catIdNum superCatIdNum
      lift $ updateInDb conn "categories" "category_name=?,super_category_id=?" "category_id=?" [catNameParam,superCatIdParam,catIdParam]
      allSuperCats <- findAllSuperCats conn catIdNum
      return . okHelper . encode $ inCatResp allSuperCats 
    ["deleteCategory"] -> do
      let paramsNames = ["category_id","admin_id","password"]
      [catIdParam,admIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [catIdNum,admIdNum]              <- mapM tryRead [catIdParam,admIdParam] :: ExceptT ReqError IO [Integer]
      isExistInDbE conn "users" "user_id=?" [admIdParam] "user_id"
      (pwd,admBool) <- selectTupleFromDbE conn "users" "user_id=?" [admIdParam] ["password","admin"]
      adminAuth pwdParam pwd admBool
      isExistInDbE conn "categories" "category_id=?" [catIdParam] "category_id"
      allSubCats <- lift $ findAllSubCats conn catIdNum
      let values = fmap (pack . show) (defCatId:allSubCats)
      let where'  = intercalate " OR " . fmap (const "post_category_id=?")  $ allSubCats
      let where'' = intercalate " OR " . fmap (const "draft_category_id=?") $ allSubCats
      lift $ updateInDb conn "posts"  "post_category_id=?"  where'  values
      lift $ updateInDb conn "drafts" "draft_category_id=?" where'' values
      let where''' = intercalate " OR " . fmap (const "category_id=?") $ allSubCats
      lift $ deleteFromDb conn "categories" where''' (fmap (pack . show) allSubCats)
      return . okHelper . encode $ OkResponse {ok = True}
    
    

checkRelationCats conn catIdNum superCatIdNum 
  |catIdNum == superCatIdNum = throwE $ SimpleError $ "super_category_id: " ++ show superCatIdNum ++ " equal to category_id."
  |otherwise                 = do
    allSubCats <- lift $ findAllSubCats conn catIdNum
    if superCatIdNum `elem` allSubCats
      then throwE $ SimpleError $ "super_category_id: " ++ show superCatIdNum ++ " is subCategory of category_id: " ++ show catIdNum
      else return ()


findAllSubCats :: Connection -> Integer -> IO [Integer]
findAllSubCats conn catId = do
  check <- isExistInDb conn "categories" "super_category_id=?" [pack . show $ catId] "category_id"
  case check of
    False -> return [catId]
    True  -> do
      xs <- selectListFromDb conn "categories" ("super_category_id",(pack . show $ catId)) "category_id"
      ys <- mapM (findAllSubCats conn) xs
      return $ catId : (Prelude.concat  ys)   
    
findAllSuperCats :: Connection -> Integer -> ExceptT ReqError IO [(Integer,Text)]
findAllSuperCats conn catId = do
  (catName,superCatId) <- selectTupleFromDbE conn "categories" "category_id=?" [pack . show $ catId] ["category_name","COALESCE (super_category_id, '0') AS super_category_id"]
  case superCatId of 
    0 -> return $ [(catId,catName)]
    _ -> do
      xs <- findAllSuperCats conn superCatId
      return $ ((catId,catName) : xs) 

deleteAllAboutDrafts conn [] = return ()
deleteAllAboutDrafts conn draftsIds = do
  let table  = "draftspics draftstags"
  let values = fmap (pack . show) draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  lift $ deleteFromDb conn table where' values
  lift $ deleteFromDb conn "drafts" where' values
  return ()

      
checkRelationUsAu conn usIdParam auIdParam = do
  check <- lift $ isExistInDb conn "authors" "user_id=?" [usIdParam] "user_id"
  case check of
    True -> do
      [Only auId] <- lift $ selectFromDb conn "authors" "user_id=?" [usIdParam] "author_id"
      case (auId :: Integer) == (read . unpack $ auIdParam) of
        True  -> return True
        False -> throwE $ SimpleError $ "user_id: " ++ unpack usIdParam ++ " is already author"
    False -> return True 

         


checkKeyE :: Text -> Text -> ExceptT ReqError IO Bool
checkKeyE keyParam key 
  | keyParam == key = return True
  | otherwise       = throwE $ SimpleError "Invalid create_admin_key"

checkParam :: (Monad m) => Request -> Text -> ExceptT ReqError m Text
checkParam req param = case lookup param $ queryToQueryText $ queryString req of
  Just (Just "") -> throwE $ SimpleError $ "Can't parse parameter:" ++ unpack param ++ ". Empty input."
  Just (Just x)  -> return x
  Just Nothing   -> throwE $ SimpleError $ "Can't parse parameter:" ++ unpack param
  Nothing -> throwE $ SimpleError $ "Can't find parameter:" ++ unpack param



tryRead :: (Read a, Monad m) => Text -> ExceptT ReqError m a
tryRead "" = throwE $ SimpleError "Can`t parse parameter. Empty input."
tryRead xs = case reads . unpack $ xs of
  [(a,"")] -> return a
  _        -> throwE $ SimpleError $ "Can`t parse value:" ++ unpack xs


--checkParam req txt = isExistParam1 req txt >>= \x -> parseParam1 req x

selectFromDb conn table where' values param = do
  xs <- query conn (fromString $ "SELECT " ++ param ++ " FROM " ++ table ++ " WHERE " ++ where' ) values
  return xs
  

selectFromDbE conn table where' values param = do
  xs <- lift $ selectFromDb conn table where' values param
  case xs of
    []           -> throwE $ SimpleError "DatabaseError.Empty output"
    [Only value] -> return value
    _            -> throwE $ SimpleError "DatabaseError"

{-
selectFromDbEText :: Connection -> String -> String -> [Text] -> String -> ExceptT ReqError IO Text
selectFromDbEText conn table where' values param = do
  xs <- lift $ (selectFromDb conn table where' values param :: IO [Only Text])
  case xs of
    [Only value] -> return value
    _            -> throwE $ SimpleError "DatabaseError"
-}

selectListFromDbE conn table where' values param = do
  xs <- lift $ selectFromDb conn table where' values param
  case xs of
    []                -> return []
    ((Only value):ys) -> return . fmap fromOnly $ xs


selectTupleFromDb conn table where' values params = do
  xs <- query conn (fromString $ "SELECT " ++ (intercalate ", " params) ++ " FROM " ++ table ++ " WHERE " ++ where' ) values
  return xs

selectTupleFromDbE conn table where' values params = do
  xs <- lift $ selectTupleFromDb conn table where' values params
  case xs of
    []      -> throwE $ SimpleError "DatabaseError.Empty output"
    [tuple] -> return tuple
    _       -> throwE $ SimpleError "DatabaseError"

selectManyListFromDbE conn table where' values params = do
  xs <- lift $ selectTupleFromDb conn table where' values params
  case xs of
    []             -> return []
    (tuple:tuples) -> return xs


func = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  let table = "drafts"
  let where' = "draft_id=5"
  let values = [] :: [Text]
  let params = ["draft_name","draft_category_id"] :: [String]
  xs  <- query conn (fromString $ "SELECT " ++ (intercalate ", " params) ++ " FROM " ++ table ++ " WHERE " ++ where' ) values :: IO [(Text,Integer)]
  print (xs :: [(Text,Integer)])

updateInDb conn table set where' values = do
  execute conn (fromString $ "UPDATE " ++ table ++ " SET " ++ set ++ " WHERE " ++ where' ) values

deleteFromDb conn table where' values = do
  execute conn (fromString $ "DELETE FROM " ++ table ++ " WHERE " ++ where') values   

isMyUrl url
  | isPrefixOf "http://localhost:3000" url = True
  | otherwise                              = False

picUrlEnd url = stripPrefix "http://localhost:3000/picture/" url

getValidMyPicUrl :: Connection -> Text -> ExceptT ReqError IO Text
getValidMyPicUrl conn url = do
  case picUrlEnd url of
    Just ""     -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url
    Just urlEnd -> return url
    Nothing     -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url 

readUrlEnd conn url = do
  picIdNum <- tryRead (fromJust . picUrlEnd $ url) :: ExceptT ReqError IO Integer
  check    <- lift $ isExistInDb conn "pics" "pic_id=?" [(pack . show $ picIdNum)] "pic_id"
  case check of 
    True  -> return picIdNum
    False -> throwE $ SimpleError $ "Invalid end of picture url:" ++ unpack url

--getIfExistInDb conn table where' values checkName = do
  --check    <- lift $ isExistInDb conn table where' values checkName
  --case check of 
    --True  -> return picIdNum
    --False -> throwE $ SimpleError $ unpack checkName ++ ": " ++ (intercalate "," . fmap unpack $ values) ++ " doesn`t exist."

getValidPicUrl :: Text -> ExceptT ReqError IO Text
getValidPicUrl url = do
  res <- (lift $ httpLBS . fromString . unpack $ url) `catch` ( (\e -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url ++ ". " ++ (show (e :: HttpException))) )
  let bs = getResponseBody res
  case decodeImage $ toStrict bs of
    Right _ -> return url
    Left _  -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url

getPicId conn url 
  |isMyUrl url = do
    picId <- (getValidMyPicUrl conn url >>= \validUrl -> readUrlEnd conn validUrl)
    return picId
  |otherwise = do
    validUrl <- getValidPicUrl url
    picId    <- lift $ insertReturnInDb conn "pics" ["pic_url"] [validUrl] "pic_id"
    return picId





isExistInDb conn table where' values checkName = do
  [Only check]  <- query conn (fromString $ "SELECT EXISTS (SELECT " ++ checkName ++ " FROM " ++ table ++ " WHERE " ++ where' ++ ")") values
  return check

isExistInDbE :: Connection -> String -> String -> [Text] -> String -> ExceptT ReqError IO Bool
isExistInDbE conn table where' values checkName = do
  check  <- lift $ isExistInDb conn table where' values checkName
  case check of
    True -> return True
    False -> throwE $ SimpleError $ checkName ++ ": " ++ (intercalate "," . fmap unpack $ values) ++ " doesn`t exist."
 
ifExistInDbThrowE :: Connection -> String -> String -> [Text] -> String -> ExceptT ReqError IO Bool
ifExistInDbThrowE conn table where' values checkName = do
  check  <- lift $ isExistInDb conn table where' values checkName
  case check of
    True -> throwE $ SimpleError $ checkName ++ ": " ++ (intercalate "," . fmap unpack $ values) ++ " already exist in " ++ table
    False -> return False


--insertReturnInDb :: Connection
insertReturnInDb conn table insNames insValues returnName = do
  [Only x] <- query conn (fromString $ "INSERT INTO " ++ table ++ " ( " ++ intercalate "," insNames ++ " ) VALUES ( " ++ (intercalate "," . fmap (const "?") $ insNames) ++ " ) RETURNING " ++ returnName) insValues
  return x

application1 req send = do
  let okHelper = send . responseBuilder status200 [("Content-Type", "application/json; charset=utf-8")]
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  case pathInfo req of
    ["createUser"]        -> do
      let passwordParam   = fromJust . fromJust . lookup     "password" $ queryToQueryText $ queryString req
      let firstNameParam  = fromJust . fromJust . lookup   "first_name" $ queryToQueryText $ queryString req
      let lastNameParam   = fromJust . fromJust . lookup    "last_name" $ queryToQueryText $ queryString req
      let userPicUrlParam = fromJust . fromJust . lookup "user_pic_url" $ queryToQueryText $ queryString req
      [Only picId]  <- query conn "INSERT INTO pics ( pic_url ) VALUES (?) RETURNING pic_id" [userPicUrlParam]
      day <- getDay  
      [Only userId] <- query conn "INSERT INTO users ( password, first_name , last_name , user_pic_id , user_create_date, admin) VALUES ( ?,?,?,?,?, false ) RETURNING user_id" [ passwordParam , firstNameParam, lastNameParam, pack (show picId), pack  day ]
      okHelper $ lazyByteString $ encode (UserResponse {user_id = userId , first_name = firstNameParam , last_name = lastNameParam, user_pic_id = picId, user_pic_url = voo picId, user_create_date = pack day })
    ["getUser", userId]      -> do
      [Only firstName] <- query conn "SELECT first_name FROM users WHERE user_id = ? " [userId ]
      [Only lastName] <- query conn "SELECT last_name FROM users WHERE user_id = ? " [userId]
      [Only picId] <- query conn "SELECT user_pic_id FROM users WHERE user_id = ? " [userId]
      [Only userCreateDate] <- (query conn "SELECT user_create_date FROM users WHERE user_id = ?" [userId]) :: IO [Only Day]
      okHelper $ lazyByteString $ encode (UserResponse {user_id = read . unpack $ userId , first_name = firstName , last_name = lastName, user_pic_id = picId, user_pic_url = voo picId, user_create_date = pack . showGregorian $ userCreateDate})
    ["deleteUser"]   -> do
      let usIdParam   = fromJust . fromJust . lookup "user_id"  $ queryToQueryText $ queryString req
      let pwdParam = fromJust . fromJust . lookup "password" $ queryToQueryText $ queryString req
      let adminIdParam  = fromJust . fromJust . lookup "admin_id" $ queryToQueryText $ queryString req
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case zoo pwdParam pwd admBool of
        "Success" -> do
          execute conn "UPDATE comments SET user_id = ? WHERE user_id = ?" [pack . show $ defUsId,usIdParam]
          [Only check]  <- query conn "SELECT EXISTS (SELECT author_id FROM authors WHERE user_id = ?)" [usIdParam]
          case check of
            True -> do
              [Only authorId] <- (query conn "SELECT author_id FROM authors WHERE user_id = ? " [usIdParam]) :: IO [Only Integer]
              execute conn "UPDATE posts SET author_id = ? WHERE author_id = ?" [pack . show $ defAuthId,pack . show $ authorId]
              onlyDraftsIds <- (query conn "SELECT draft_id FROM drafts WHERE author_id = ? " [pack . show $ authorId]) :: IO [Only Integer]
              mapM boo $ fmap fromOnly onlyDraftsIds
              mapM doo $ fmap fromOnly onlyDraftsIds
              mapM hoo $ fmap fromOnly onlyDraftsIds
              execute conn "DELETE FROM authors WHERE author_id = ?" [pack . show $ authorId]
              execute conn "DELETE FROM users WHERE user_id = ?" [usIdParam]
              okHelper $ lazyByteString $ encode (OkResponse {ok = True})
            False -> do
              execute conn "DELETE FROM users WHERE user_id = ?" [usIdParam]
              okHelper $ lazyByteString $ encode (OkResponse {ok = True})
        "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["createAdmin"]        -> do
      let createAdminKey  = fromJust . fromJust . lookup "create_admin_key" $ queryToQueryText $ queryString req
      let passwordParam   = fromJust . fromJust . lookup         "password" $ queryToQueryText $ queryString req
      let firstNameParam  = fromJust . fromJust . lookup       "first_name" $ queryToQueryText $ queryString req
      let lastNameParam   = fromJust . fromJust . lookup        "last_name" $ queryToQueryText $ queryString req
      let userPicUrlParam = fromJust . fromJust . lookup     "user_pic_url" $ queryToQueryText $ queryString req
      [Only key]  <- query_ conn "SELECT create_admin_key FROM key"
      case  createAdminKey == key of
        True -> do
          [Only picId]  <- query conn "INSERT INTO pics ( pic_url ) VALUES (?) RETURNING pic_id" [userPicUrlParam]
          day <- getDay  
          [Only userId] <- query conn "INSERT INTO users ( password, first_name , last_name , user_pic_id , user_create_date, admin) VALUES ( ?,?,?,?,?, true ) RETURNING user_id" [ passwordParam , firstNameParam, lastNameParam, pack (show picId), pack day ]
          okHelper $ lazyByteString $ encode (UserResponse {user_id = userId , first_name = firstNameParam , last_name = lastNameParam, user_pic_id = picId, user_pic_url = voo picId,user_create_date = pack day })
        False -> send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["createAuthor"]        -> do
      let adminIdParam    = fromJust . fromJust . lookup "admin_id"    $ queryToQueryText $ queryString req
      let pwdParam   = fromJust . fromJust . lookup "password"    $ queryToQueryText $ queryString req
      let userIdParam     = fromJust . fromJust . lookup "user_id"     $ queryToQueryText $ queryString req
      let authorInfoParam = fromJust . fromJust . lookup "author_info" $ queryToQueryText $ queryString req
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case zoo pwdParam pwd admBool of
        "Success" -> do
          [Only authorId] <- query conn "INSERT INTO authors ( user_id , author_info) VALUES ( ?,?) RETURNING author_id" [ userIdParam, authorInfoParam]
          okHelper $ lazyByteString $ encode (AuthorResponse { author_id = authorId , auth_user_id = read $ unpack $ userIdParam , author_info = authorInfoParam})
        "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["getAuthor"]        -> do
      let adminIdParam    = fromJust . fromJust . lookup "admin_id"    $ queryToQueryText $ queryString req
      let pwdParam   = fromJust . fromJust . lookup "password"    $ queryToQueryText $ queryString req
      let authorIdParam = fromJust . fromJust . lookup "author_id" $ queryToQueryText $ queryString req
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case zoo pwdParam pwd admBool of
        "Success" -> do
          [Only authorInfo] <- query conn "SELECT author_info FROM authors WHERE author_id = ?" [authorIdParam]
          [Only userId] <- query conn "SELECT user_id FROM authors WHERE author_id = ?" [authorIdParam]
          okHelper $ lazyByteString $ encode (AuthorResponse { author_id = read . unpack $ authorIdParam , auth_user_id = userId , author_info = authorInfo})
        "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["updateAuthor"]        -> do
      let adminIdParam    = fromJust . fromJust . lookup "admin_id"    $ queryToQueryText $ queryString req
      let pwdParam   = fromJust . fromJust . lookup "password"    $ queryToQueryText $ queryString req
      let authorIdParam = fromJust . fromJust . lookup "author_id" $ queryToQueryText $ queryString req
      let authorInfoParam = fromJust . fromJust . lookup "author_info" $ queryToQueryText $ queryString req
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case zoo pwdParam pwd admBool of
        "Success" -> do
          [Only userId] <- query conn "SELECT user_id FROM authors WHERE author_id = ?" [authorIdParam]
          execute conn "UPDATE authors SET author_info = ? WHERE author_id = ?" [authorInfoParam,authorIdParam]
          okHelper $ lazyByteString $ encode (AuthorResponse { author_id = read . unpack $ authorIdParam , auth_user_id = userId , author_info = authorInfoParam})
        "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["deleteAuthor"]   -> do
      let adminIdParam  = fromJust . fromJust . lookup "admin_id" $ queryToQueryText $ queryString req
      let pwdParam = fromJust . fromJust . lookup "password" $ queryToQueryText $ queryString req
      let authIdParam   = fromJust . fromJust . lookup "author_id"  $ queryToQueryText $ queryString req   
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case zoo pwdParam pwd admBool of
        "Success" -> do
          [Only check]  <- query conn "SELECT EXISTS (SELECT author_id FROM authors WHERE author_id = ?)" [authIdParam]
          case check of
            True -> do
              execute conn "UPDATE posts SET author_id = ? WHERE author_id = ?" [pack . show $ defAuthId,authIdParam]
              onlyDraftsIds <- (query conn "SELECT draft_id FROM drafts WHERE author_id = ? " [authIdParam]) :: IO [Only Integer]
              mapM boo $ fmap fromOnly onlyDraftsIds
              mapM doo $ fmap fromOnly onlyDraftsIds
              mapM hoo $ fmap fromOnly onlyDraftsIds
              execute conn "DELETE FROM authors WHERE author_id = ?" [authIdParam]
              okHelper $ lazyByteString $ encode (OkResponse {ok = True})
            False -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "author_id:" ++ unpack authIdParam ++ " doesn`t exist"}
        "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["createCategory"]        -> do
      let adminIdParam    = fromJust . fromJust . lookup "admin_id"      $ queryToQueryText $ queryString req
      let pwdParam   = fromJust . fromJust . lookup "password"      $ queryToQueryText $ queryString req
      let catNameParam    = fromJust . fromJust . lookup "category_name" $ queryToQueryText $ queryString req
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case zoo pwdParam pwd admBool of
        "Success" -> do
          [Only catId] <- query conn "INSERT INTO categories ( category_name) VALUES (?) RETURNING category_id " [ catNameParam]
          [Only superCatId] <- query conn "SELECT COALESCE (super_category_id, '0') AS super_category_id FROM categories WHERE category_id = ?" [pack . show $ catId]
          okHelper $ lazyByteString $ encode (CatResponse { cat_id = catId , cat_name =  catNameParam , super_cat = pack . prettyNull . show $ (superCatId :: Int)})
        "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["createSubCategory"]        -> do
      let adminIdParam    = fromJust . fromJust . lookup "admin_id"          $ queryToQueryText $ queryString req
      let pwdParam   = fromJust . fromJust . lookup "password"          $ queryToQueryText $ queryString req
      let catNameParam    = fromJust . fromJust . lookup "category_name"     $ queryToQueryText $ queryString req
      let superCatIdParam = fromJust . fromJust . lookup "super_category_id" $ queryToQueryText $ queryString req
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case zoo pwdParam pwd admBool of
        "Success" -> do
          [Only catId] <- query conn "INSERT INTO categories ( category_name, super_category_id) VALUES (?,?) RETURNING category_id" [ catNameParam, superCatIdParam ]
          xs <- foo catId
          okHelper $ lazyByteString $ encode $ inCatResp xs
        "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["getCategory", catId] -> do
      xs <- foo (read $ unpack $ catId)
      okHelper $ lazyByteString $ encode $ inCatResp xs
    ["updateCategory"] -> do
      case fmap (isExistParam req) ["admin_id","password","category_id","category_name","super_category_id"] of
        [True,True,True,True,True] -> do
          case fmap (parseParam req) ["admin_id","password","category_id","category_name","super_category_id"] of
            [Just adminIdParam,Just pwdParam,Just catIdParam,Just catNameParam,Just superCatIdParam] -> do
              [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
              [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
              case zoo pwdParam pwd admBool of
                "Success" -> do
                  updateDb conn "categories" ("category_name", catNameParam) "category_id" catIdParam
                  updateDb conn "categories" ("super_category_id", superCatIdParam) "category_id" catIdParam
                  allSuperCats <- foo (read $ unpack $ catIdParam)  
                  okHelper $ lazyByteString $ encode $ inCatResp allSuperCats
                "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
                "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
                "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
            _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t parse query parameter"}
        _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t find query parameter"} 
    ["deleteCategory"] -> do
      case fmap (isExistParam req) ["admin_id","password","category_id"] of
        [True,True,True] -> do
          case fmap (parseParam req) ["admin_id","password","category_id"] of
            [Just adminIdParam,Just pwdParam,Just catIdParam] -> do
              [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
              [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
              case zoo pwdParam pwd admBool of
                "Success" -> do
                  let catIdNum = read . unpack $ catIdParam
                  xs <- findAllSubCat catIdNum
                  let allSubCats = fmap (pack . show) . Prelude.reverse $ xs
                  mapM (updateDb conn "posts" ("post_category_id", pack . show $ defCatId) "post_category_id") allSubCats
                  mapM (updateDb conn "drafts" ("draft_category_id", pack . show $ defCatId) "draft_category_id") allSubCats
                  mapM (deleteFromDb1 conn "categories" "category_id") allSubCats
                  okHelper $ lazyByteString $ encode (OkResponse { ok = True })
                "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
                "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
                "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
            _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t parse query parameter"}
        _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t find query parameter"} 
    ["createTag"]  -> do
      let adminIdParam    = fromJust . fromJust . lookup "admin_id"          $ queryToQueryText $ queryString req
      let pwdParam   = fromJust . fromJust . lookup "password"          $ queryToQueryText $ queryString req
      let tagNameParam    = fromJust . fromJust . lookup "tag_name"     $ queryToQueryText $ queryString req
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case zoo pwdParam pwd admBool of
        "Success" -> do
          [Only tagId] <- query conn "INSERT INTO tags ( tag_name) VALUES (?) RETURNING tag_id" [ tagNameParam ]
          okHelper $ lazyByteString $ encode $ TagResponse tagId tagNameParam
        "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["getTag",tagId]  -> do
      [Only tagName] <- query conn "SELECT tag_name FROM tags WHERE tag_id = ? " [ tagId ]
      okHelper $ lazyByteString $ encode $ TagResponse (read . unpack $ tagId) tagName
    ["updateTag"]        -> do
      let adminIdParam    = fromJust . fromJust . lookup "admin_id"    $ queryToQueryText $ queryString req
      let pwdParam   = fromJust . fromJust . lookup "password"    $ queryToQueryText $ queryString req
      let tagIdParam = fromJust . fromJust . lookup "tag_id" $ queryToQueryText $ queryString req
      let tagNameParam = fromJust . fromJust . lookup "tag_name" $ queryToQueryText $ queryString req
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case zoo pwdParam pwd admBool of
        "Success" -> do
          execute conn "UPDATE tags SET tag_name = ? WHERE tag_id = ?" [tagNameParam,tagIdParam]
          okHelper $ lazyByteString $ encode (TagResponse { tag_id = read . unpack $ tagIdParam , tag_name = tagNameParam})
        "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["deleteTag"]        -> do
      case fmap (isExistParam req) ["admin_id","password","tag_id"] of
        [True,True,True] -> do
          case fmap (parseParam req) ["admin_id","password","tag_id"] of
            [Just adminIdParam,Just pwdParam,Just tagIdParam] -> do
              [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
              [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
              case zoo pwdParam pwd admBool of
                "Success" -> do
                  execute conn "DELETE FROM draftstags WHERE tag_id = ?" [tagIdParam]
                  execute conn "DELETE FROM poststags WHERE tag_id = ?" [tagIdParam]
                  execute conn "DELETE FROM tags WHERE tag_id = ?" [tagIdParam]
                  okHelper $ lazyByteString $ encode (OkResponse { ok = True })
                "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
                "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
                "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
            _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t parse query parameter"}
        _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t find query parameter"} 
    ["createNewDraft"]  -> do
      body <- strictRequestBody req
      let usIdParam = user_id1 . fromJust . decode $ body
      let pwdParam  = password1 . fromJust . decode $ body
      let draftNameParam  = draft_name . fromJust . decode $ body
      let draftCatIdParam  = draft_cat_id . fromJust . decode $ body
      let draftTextParam  = draft_text1 . fromJust . decode $ body
      let draftMainPicUrlParam  = draft_main_pic_url . fromJust . decode $ body
      let draftTagsIds  = fmap tag_id3 . draft_tags_ids . fromJust . decode $ body
      let draftPicsUrls  = fmap pic_url . draft_pics_urls . fromJust . decode $ body
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [usIdParam]
      case pwdParam == pwd of
        True -> do
          [Only check]  <- query conn "SELECT EXISTS (SELECT author_id FROM authors WHERE user_id = ?)" [usIdParam]
          case check of
            True -> do
              [Only authorId] <- (query conn "SELECT author_id FROM authors WHERE user_id = ?" [usIdParam]) :: IO [Only Integer]
              [Only authorInfo] <- query conn "SELECT author_info FROM authors WHERE user_id = ?" [usIdParam]
              [Only picId]  <- query conn "INSERT INTO pics ( pic_url ) VALUES (?) RETURNING pic_id" [draftMainPicUrlParam]
              [Only draftId] <- query conn "INSERT INTO drafts (author_id, draft_name, draft_category_id, draft_text, draft_main_pic_id) VALUES (?,?,?,?,?) RETURNING draft_id" [pack . show $ authorId,draftNameParam,pack . show $ draftCatIdParam,draftTextParam,pack . show $ picId]
              mapM (koo draftId) draftTagsIds
              xs <- foo draftCatIdParam
              draftPicsIds <- mapM goo draftPicsUrls
              mapM (poo draftId) draftPicsIds
              ys <- mapM roo draftTagsIds
              okHelper $ lazyByteString $ encode $ DraftResponse { draft_id2 = draftId, post_id2 = PostText "NULL" , author2 = AuthorResponse authorId usIdParam authorInfo, draft_name2 = draftNameParam , draft_cat2 =  inCatResp xs , draft_text2 = draftTextParam , draft_main_pic_id2 =  picId , draft_main_pic_url2 = voo picId , draft_tags2 = ys, draft_pics2 =  loo draftPicsIds (fmap voo draftPicsIds)}
            False -> okHelper $ lazyByteString $ encode (OkResponse {ok = False})
        False -> okHelper $ lazyByteString $ encode (OkResponse {ok = False})
    ["createPostsDraft"]  -> do
      let usIdParam   = fromJust . fromJust . lookup "user_id"          $ queryToQueryText $ queryString req
      let pwdParam = fromJust . fromJust . lookup "password"          $ queryToQueryText $ queryString req
      let postIdParam  = fromJust . fromJust . lookup "post_id"     $ queryToQueryText $ queryString req
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [usIdParam]
      case pwdParam == pwd of
        True -> do
          [Only check]  <- query conn "SELECT EXISTS (SELECT author_id FROM authors WHERE user_id = ?)" [usIdParam]
          case check of
            True -> do
              [Only authorUsId] <- (query conn "SELECT author_id FROM authors WHERE user_id = ?" [usIdParam]) :: IO [Only Integer]
              [Only authorPostId] <- (query conn "SELECT author_id FROM posts WHERE post_id = ?" [postIdParam]) :: IO [Only Integer]
              case (authorUsId == authorPostId) of
                True -> do
                  [Only postName] <- query conn "SELECT post_name FROM posts WHERE post_id = ?" [postIdParam]
                  [Only postText] <- query conn "SELECT post_text FROM posts WHERE post_id = ?" [postIdParam]
                  [Only postCatId] <- query conn "SELECT post_category_id  FROM posts WHERE post_id = ?" [postIdParam]
                  [Only postMainPicId] <- query conn "SELECT post_main_pic_id FROM posts WHERE post_id = ?" [postIdParam]                  
                  [Only draftId] <- query conn "INSERT INTO drafts (post_id, author_id, draft_name, draft_category_id, draft_text, draft_main_pic_id) VALUES (?,?,?,?,?,?) RETURNING draft_id" [postIdParam,pack . show $ authorUsId,postName,pack . show $ postCatId,postText,pack . show $ postMainPicId]
                  [Only authorInfo] <- query conn "SELECT author_info FROM authors WHERE user_id = ?" [usIdParam]
                  xs <- query conn "SELECT pic_id FROM postspics WHERE post_id = ?" [postIdParam]
                  let picsIds = fmap fromOnly xs
                  ys <- query conn "SELECT tag_id FROM poststags WHERE post_id = ?" [postIdParam] 
                  let tagsIds = fmap fromOnly ys
                  mapM (koo draftId) tagsIds
                  mapM (poo draftId) picsIds
                  zs <- foo postCatId
                  hs <- mapM roo tagsIds
                  okHelper $ lazyByteString $ encode $ DraftResponse { draft_id2 = draftId, post_id2 = PostInteger $ read . unpack $ postIdParam, author2 = AuthorResponse authorUsId (read . unpack $ usIdParam) authorInfo  , draft_name2 = postName , draft_cat2 =  inCatResp zs , draft_text2 = postText , draft_main_pic_id2 =  postMainPicId , draft_main_pic_url2 = voo postMainPicId , draft_tags2 = hs, draft_pics2 =  loo picsIds (fmap voo picsIds)}
                False -> okHelper $ lazyByteString $ encode (OkResponse {ok = False})
            False -> okHelper $ lazyByteString $ encode (OkResponse {ok = False})
        False -> okHelper $ lazyByteString $ encode (OkResponse {ok = False})
    ["getDraft"]  -> do
      case fmap (isExistParam req) ["user_id","password","draft_id"] of
        [True,True,True] -> do
          case fmap (parseParam req) ["user_id","password","draft_id"] of
            [Just usIdParam,Just pwdParam,Just draftIdParam] -> do
              [Only usPwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [usIdParam]
              case pwdParam == usPwd of
                True -> do
                  authorId <- selectFromDb1 conn "drafts" ("draft_id",draftIdParam) "author_id" :: IO Integer
                  usDraftId <- selectFromDb1 conn "authors" ("author_id",(pack . show $ authorId)) "user_id" :: IO Integer
                  case usDraftId == (read . unpack $ usIdParam) of
                    True -> do
                      [Only postId] <- query conn "SELECT COALESCE (post_id, '0') AS post_id FROM drafts WHERE draft_id = ?" [draftIdParam]
                      [draftCatId,draftPicId] <- mapM (selectFromDb1 conn "drafts" ("draft_id",draftIdParam)) ["draft_category_id","draft_main_pic_id"]
                      [draftName,draftText] <- mapM (selectFromDb1 conn "drafts" ("draft_id",draftIdParam)) ["draft_name","draft_text"]
                      authorInfo <- selectFromDb1 conn "authors" ("author_id",pack . show $ authorId) "author_info" 
                      allSuperCats <- foo draftCatId  
                      draftPicsIds <- selectListFromDb conn "draftspics" ("draft_id",draftIdParam) "pic_id"
                      draftTagsIds <- selectListFromDb conn "draftstags" ("draft_id",draftIdParam) "tag_id"
                      hs <- mapM roo draftTagsIds
                      okHelper $ lazyByteString $ encode $ DraftResponse { draft_id2 = read . unpack $ draftIdParam, post_id2 = (\pId -> if pId == 0 then PostText "NULL" else PostInteger pId) postId , author2 = AuthorResponse authorId (read . unpack $ usIdParam) authorInfo, draft_name2 = draftName , draft_cat2 =  inCatResp allSuperCats , draft_text2 = draftText , draft_main_pic_id2 =  draftPicId , draft_main_pic_url2 = voo draftPicId , draft_tags2 = hs, draft_pics2 =  loo draftPicsIds (fmap voo draftPicsIds)}   
                    False ->  okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "User cannot get this draft"}
                False ->  okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "INVALID password"}
            _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t parse query parameter"}
        _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t find query parameter"}    
    ["getDrafts"]  -> do
      case fmap (isExistParam req) ["user_id","password","page"] of
        [True,True,True] -> do
          case fmap (parseParam req) ["user_id","password","page"] of
            [Just usIdParam,Just pwdParam,Just pageParam] -> do
              [Only usPwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [usIdParam]
              case pwdParam == usPwd of
                True -> do
                  [Only isAuthor]  <- query conn "SELECT EXISTS (SELECT author_id FROM authors WHERE user_id = ?)" [usIdParam]
                  case isAuthor of
                    True -> do
                      authorId <- selectFromDb1 conn "authors" ("user_id",usIdParam) "author_id" :: IO Integer
                      [Only isExistDraft]  <- query conn "SELECT EXISTS (SELECT draft_id FROM drafts WHERE author_id = ?)" [(pack . show $ authorId)]
                      case isExistDraft of
                        True -> do
                          let pageNum = read . unpack $ pageParam
                          let table = "drafts JOIN authors ON authors.author_id = drafts.author_id"
                          let orderBy = "draft_id DESC"
                          let extractParams = ["draft_id","COALESCE (post_id, '0') AS post_id","draft_name","draft_category_id","draft_text","draft_main_pic_id","author_info"]
                          let where' = "drafts.author_id = ?"
                          let values = [pack . show $ authorId]
                          params <- selectManyOrderLimitWhereFromDb conn table orderBy pageNum draftNumberLimit extractParams where' values   
                          let alldraftIdsText = fmap (pack . show . firstSeven) params
                          let allCatIdsNum = fmap fourthSeven params
                          manyAllSuperCats <- mapM foo allCatIdsNum
                          manyDraftPicsIds <- mapM (reverseSelectListFromDb conn "draftspics" "pic_id" "draft_id") alldraftIdsText  
                          manyDraftTagsIds <- mapM (reverseSelectListFromDb conn "draftstags" "tag_id" "draft_id") alldraftIdsText  
                          hss <- mapM (mapM roo) manyDraftTagsIds
                          let allParams = zip4 params manyAllSuperCats manyDraftPicsIds hss
                          okHelper $ lazyByteString $ encode $ DraftsResponse { page9 = pageNum , drafts9 = fmap (\((draftId,postId,draftName,draftCat,draftText,draftMainPicId,auInfo),cats,pics,tags) -> DraftResponse { draft_id2 = draftId, post_id2 = (\pId -> if pId == 0 then PostText "NULL" else PostInteger pId) postId , author2 = AuthorResponse authorId (read . unpack $ usIdParam) auInfo, draft_name2 = draftName , draft_cat2 =  inCatResp cats , draft_text2 = draftText , draft_main_pic_id2 =  draftMainPicId , draft_main_pic_url2 = voo draftMainPicId , draft_tags2 = tags, draft_pics2 =  loo pics (fmap voo pics)}) allParams }   
                        False ->  okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Author has not draft"}
                    False ->  okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "User cannot get drafts"}
                False ->  okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "INVALID password"}
            _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t parse query parameter"}
        _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t find query parameter"}    
    ["updateDraft",draftId]  -> do
      let draftIdNum = read . unpack $ draftId
      body <- strictRequestBody req
      let usIdParam = user_id1 . fromJust . decode $ body
      let pwdParam  = password1 . fromJust . decode $ body
      let draftNameParam  = draft_name . fromJust . decode $ body
      let draftCatIdParam  = draft_cat_id . fromJust . decode $ body
      let draftTextParam  = draft_text1 . fromJust . decode $ body
      let draftMainPicUrlParam  = draft_main_pic_url . fromJust . decode $ body
      let draftTagsIds  = fmap tag_id3 . draft_tags_ids . fromJust . decode $ body
      let draftPicsUrls  = fmap pic_url . draft_pics_urls . fromJust . decode $ body
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [usIdParam]
      case pwdParam == pwd of
        True -> do
          authorUsId <- selectFromDb1 conn "authors" ("user_id",pack .show $ usIdParam) "author_id" :: IO Integer
          authorDraftId <- selectFromDb1 conn "drafts" ("draft_id",draftId) "author_id" :: IO Integer
          case (authorUsId == authorDraftId) of
            True -> do
              [Only authorInfo] <- query conn "SELECT author_info FROM authors WHERE user_id = ?" [pack .show $ usIdParam]
              [Only picId]  <- query conn "INSERT INTO pics ( pic_url ) VALUES (?) RETURNING pic_id" [draftMainPicUrlParam]
              execute conn "DELETE FROM draftstags WHERE draft_id = ?" [draftId]
              execute conn "DELETE FROM draftspics WHERE draft_id = ?" [draftId]
              execute conn "UPDATE drafts SET draft_name = ?, draft_category_id = ?, draft_text = ?, draft_main_pic_id = ? WHERE draft_id = ?" [draftNameParam,pack . show $ draftCatIdParam,draftTextParam,pack . show $ picId,draftId]
              mapM (koo draftIdNum) draftTagsIds
              ys <- mapM roo draftTagsIds
              draftPicsIds <- mapM goo draftPicsUrls
              mapM (poo draftIdNum) draftPicsIds
              xs <- foo draftCatIdParam
              [Only postId]   <- (query conn "SELECT COALESCE (post_id, '0') AS post_id FROM drafts WHERE draft_id = ?" [draftId]) :: IO [Only Integer]
              okHelper $ lazyByteString $ encode $ DraftResponse { draft_id2 = draftIdNum, post_id2 = (\pId -> if pId == 0 then PostText "NULL" else PostInteger pId) postId , author2 = AuthorResponse authorDraftId usIdParam authorInfo, draft_name2 = draftNameParam , draft_cat2 =  inCatResp xs , draft_text2 = draftTextParam , draft_main_pic_id2 =  picId , draft_main_pic_url2 = voo picId , draft_tags2 = ys, draft_pics2 =  loo draftPicsIds (fmap voo draftPicsIds)}
            False -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "User cannot update this draft"}
        False ->  okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "INVALID password"}
    ["deleteDraft"]  -> do
      case fmap (isExistParam req) ["user_id","password","draft_id"] of
        [True,True,True] -> do
          case fmap (parseParam req) ["user_id","password","draft_id"] of
            [Just usIdParam,Just pwdParam,Just draftIdParam] -> do
              [Only usPwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [usIdParam]
              case pwdParam == usPwd of
                True -> do
                  authorId <- selectFromDb1 conn "drafts" ("draft_id",draftIdParam) "author_id" :: IO Integer
                  usDraftId <- selectFromDb1 conn "authors" ("author_id",(pack . show $ authorId)) "user_id" :: IO Integer
                  case usDraftId == (read . unpack $ usIdParam) of
                    True -> do
                      execute conn "DELETE FROM draftstags WHERE draft_id = ?" [draftIdParam]
                      execute conn "DELETE FROM draftspics WHERE draft_id = ?" [draftIdParam]
                      execute conn "DELETE FROM drafts WHERE draft_id = ?" [draftIdParam]
                      okHelper $ lazyByteString $ encode (OkResponse { ok = True })
                    False ->  okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "User cannot delete this draft"}
                False ->  okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "INVALID password"}
            _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t parse query parameter"}
        _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t find query parameter"}    
    ["publishDraft"]  -> do
      let usIdParam   = fromJust . fromJust . lookup "user_id"          $ queryToQueryText $ queryString req
      let passwordParam = fromJust . fromJust . lookup "password"          $ queryToQueryText $ queryString req
      let draftIdParam  = fromJust . fromJust . lookup "draft_id"     $ queryToQueryText $ queryString req
      [Only check]  <- query conn "SELECT EXISTS (SELECT author_id FROM authors WHERE user_id = ?)" [usIdParam]
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [usIdParam]
      [Only authorUsId] <- (query conn "SELECT author_id FROM authors WHERE user_id = ?" [usIdParam]) :: IO [Only Integer]
      [Only authorDraftId] <- (query conn "SELECT author_id FROM drafts WHERE draft_id = ?" [draftIdParam]) :: IO [Only Integer]
      [Only postId]   <- (query conn "SELECT COALESCE (post_id, '0') AS post_id FROM drafts WHERE draft_id = ?" [draftIdParam]) :: IO [Only Integer]
      case (passwordParam == pwd) && (authorUsId == authorDraftId) && check of
        True  -> case postId of
          0 -> do
            [Only draftName] <- query conn "SELECT draft_name FROM drafts WHERE draft_id = ?" [draftIdParam]
            [Only draftCatId] <- query conn "SELECT draft_category_id FROM drafts WHERE draft_id = ?" [draftIdParam]
            [Only draftText] <- query conn "SELECT draft_text FROM drafts WHERE draft_id = ?" [draftIdParam]
            [Only draftMainPicId] <- query conn "SELECT draft_main_pic_id FROM drafts WHERE draft_id = ?" [draftIdParam]
            [Only authorInfo] <- query conn "SELECT author_info FROM authors WHERE user_id = ?" [usIdParam]
            day <- getDay
            [Only postId] <- query conn "INSERT INTO posts (author_id, post_name, post_create_date, post_category_id, post_text, post_main_pic_id) VALUES (?,?,?,?,?,?) RETURNING post_id" [pack . show $ authorUsId,draftName,pack day,pack . show $ draftCatId,draftText,pack . show $ draftMainPicId]
            xs <- query conn "SELECT pic_id FROM draftspics WHERE draft_id = ?" [draftIdParam]
            let picsIds = fmap fromOnly xs
            mapM (qoo postId) picsIds
            ys <- query conn "SELECT tag_id FROM draftstags WHERE draft_id = ?" [draftIdParam] 
            let tagsIds = fmap fromOnly ys
            mapM (woo postId) tagsIds
            zs <- foo draftCatId
            hs <- mapM roo tagsIds
            okHelper $ lazyByteString $ encode $ PostResponse { post_id = postId, author4 = AuthorResponse authorUsId (read . unpack $ usIdParam) authorInfo, post_name = draftName , post_create_date = (pack day), post_cat = inCatResp zs, post_text = draftText, post_main_pic_id = draftMainPicId, post_main_pic_url = voo draftMainPicId, post_pics = loo picsIds (fmap voo picsIds), post_tags = hs}
          _ -> do 
            [Only postId] <- query conn "SELECT post_id FROM drafts WHERE draft_id = ?" [draftIdParam]
            [Only draftName] <- query conn "SELECT draft_name FROM drafts WHERE draft_id = ?" [draftIdParam]
            [Only draftCatId] <- query conn "SELECT draft_category_id FROM drafts WHERE draft_id = ?" [draftIdParam]
            [Only draftText] <- query conn "SELECT draft_text FROM drafts WHERE draft_id = ?" [draftIdParam]
            [Only draftMainPicId] <- query conn "SELECT draft_main_pic_id FROM drafts WHERE draft_id = ?" [draftIdParam]
            [Only authorInfo] <- query conn "SELECT author_info FROM authors WHERE user_id = ?" [usIdParam]
            xs <- query conn "SELECT pic_id FROM draftspics WHERE draft_id = ?" [draftIdParam]
            let picsIds = fmap fromOnly xs
            ys <- query conn "SELECT tag_id FROM draftstags WHERE draft_id = ?" [draftIdParam] 
            let tagsIds = fmap fromOnly ys
            execute conn "UPDATE posts SET post_name = ?, post_category_id = ?, post_text = ? , post_main_pic_id = ? WHERE post_id = ?" [draftName,pack . show $ draftCatId,draftText,pack . show $ draftMainPicId,pack . show $ postId]
            [Only postCreateDate] <- (query conn "SELECT post_create_date FROM posts WHERE post_id = ?" [pack . show $ postId]) :: IO [Only Day]
            execute conn "DELETE FROM poststags WHERE post_id = ?" [pack . show $ postId]
            execute conn "DELETE FROM postspics WHERE post_id = ?" [pack . show $ postId]
            mapM (woo postId) tagsIds
            mapM (qoo postId) picsIds
            zs <- foo draftCatId
            hs <- mapM roo tagsIds
            okHelper $ lazyByteString $ encode $ PostResponse { post_id = postId, author4 = AuthorResponse authorUsId (read . unpack $ usIdParam) authorInfo, post_name = draftName , post_create_date = pack . showGregorian $ postCreateDate, post_cat = inCatResp zs, post_text = draftText, post_main_pic_id = draftMainPicId, post_main_pic_url = voo draftMainPicId, post_pics = loo picsIds (fmap voo picsIds), post_tags = hs}
        False -> okHelper $ lazyByteString $ encode (OkResponse {ok = False})
    ["getPost",postId]  -> do
      [Only isExistPost]  <- query conn "SELECT EXISTS (SELECT post_id FROM posts WHERE post_id = ?)" [postId]
      case isExistPost of
        True -> do  
          [(auId,usId,auInfo,pName,pDate,pCatId,pText,picId)] <- selectManyWhereFromDb conn "posts JOIN authors ON authors.author_id = posts.author_id " ("post_id",postId) ["posts.author_id","user_id","author_info","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"]
          allSuperCats <- foo pCatId
          picsIds <- selectListFromDb conn "postspics" ("post_id",postId) "pic_id"
          tagsIds <- selectListFromDb conn "poststags" ("post_id",postId) "tag_id"
          hs <- mapM roo tagsIds
          okHelper $ lazyByteString $ encode $ PostResponse { post_id = (read . unpack $ postId), author4 = AuthorResponse auId usId auInfo, post_name = pName , post_create_date = pack . showGregorian $ pDate, post_cat = inCatResp allSuperCats, post_text = pText, post_main_pic_id = picId, post_main_pic_url = voo picId, post_pics = loo picsIds (fmap voo picsIds), post_tags = hs}
        False ->  okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Post id:" ++ unpack postId ++ " doesn`t exist"}
    ["getPosts", page] -> do
      let pageNum = read . unpack $ page :: Integer
      let extractParamsList = ["posts.post_id","posts.author_id","authors.user_id","author_info","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"]
      case chooseArgs req of
        Right (table,where',orderBy,values)-> do
          params <- selectManyOrderLimitWhereFromDb conn table orderBy pageNum postNumberLimit extractParamsList where' values 
          let postIdsText = fmap (pack . show . firstNine) params
          let postCatsIds = fmap seventhNine params 
          manySuperCats <- mapM foo postCatsIds
          manyPostPicsIds <- mapM (reverseSelectListFromDb conn "postspics" "pic_id" "post_id") postIdsText  
          manyPostTagsIds <- mapM (reverseSelectListFromDb conn "poststags" "tag_id" "post_id") postIdsText  
          hss <- mapM (mapM roo) manyPostTagsIds
          let allParams = zip4 params manySuperCats manyPostPicsIds hss
          okHelper $ lazyByteString $ encode $ PostsResponse {page10 = pageNum , posts10 = fmap (\((pId,auId,usId,auInfo,pName,pDate,pCat,pText,picId),cats,pics,tags) -> PostResponse { post_id = pId, author4 = AuthorResponse auId usId auInfo, post_name = pName , post_create_date = pack . showGregorian $ pDate, post_cat = inCatResp cats, post_text = pText, post_main_pic_id = picId, post_main_pic_url = voo picId, post_pics = loo pics (fmap voo pics), post_tags = tags}) allParams}
        Left str -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack str}
    ["deletePost"]  -> do
      case fmap (isExistParam req) ["admin_id","password","post_id"] of
        [True,True,True] -> do
          case fmap (parseParam req) ["admin_id","password","post_id"] of
            [Just adminIdParam,Just pwdParam,Just postIdParam] -> do
              [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
              [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
              case zoo pwdParam pwd admBool of
                "Success" -> do
                  execute conn "DELETE FROM poststags WHERE post_id = ?" [postIdParam]
                  execute conn "DELETE FROM postspics WHERE post_id = ?" [postIdParam]
                  execute conn "DELETE FROM comments  WHERE post_id = ?" [postIdParam]
                  execute conn "DELETE FROM drafts    WHERE post_id = ?" [postIdParam]
                  execute conn "DELETE FROM posts     WHERE post_id = ?" [postIdParam]
                  okHelper $ lazyByteString $ encode (OkResponse { ok = True })
                "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
                "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
                "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
            _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t parse query parameter"}
        _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t find query parameter"}      
    ["createComment"]  -> do
      case fmap (isExistParam req) ["user_id","password","post_id","comment_text"] of
        [True,True,True,True] -> do
          case fmap (parseParam req) ["user_id","password","post_id","comment_text"] of
            [Just usIdParam,Just pwdParam,Just postIdParam,Just commentTextParam] -> do
              [Only usPwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [usIdParam]
              case pwdParam == usPwd of
                True -> do
                  [Only commentId] <- query conn "INSERT INTO comments (comment_text, post_id, user_id) VALUES (?,?,?) RETURNING comment_id" [ commentTextParam, postIdParam, usIdParam ]
                  okHelper $ lazyByteString $ encode $ CommentResponse { comment_id = commentId , comment_text = commentTextParam, post_id6 = read . unpack $ postIdParam, user_id6 = read . unpack $ usIdParam}
                False ->  okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "INVALID password"}
            _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t parse query parameter"}
        _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t find query parameter"}
    ["getComments"] -> do
      case fmap (isExistParam req) ["post_id","page"] of
        [True,True] -> do
          case fmap (parseParam req) ["post_id","page"] of
            [Just postIdParam, Just pageParam] -> do
              let pageNum = read . unpack $ pageParam
              xs <- (query conn (fromString $ "SELECT comment_id, comment_text, user_id FROM comments WHERE post_id = ? OFFSET " ++ show ((pageNum-1)*commentNumberLimit) ++ " LIMIT " ++ show (pageNum*commentNumberLimit)) [postIdParam]) :: IO [(Integer,Text,Integer)]
              okHelper $ lazyByteString $ encode $ CommentsResponse {page = pageNum, post_id9 = (read . unpack $ postIdParam), comments = coo xs }
    ["updateMyComment"]  -> do
      case fmap (isExistParam req) ["user_id","password","comment_id","comment_text"] of
        [True,True,True,True] -> do
          case fmap (parseParam req) ["user_id","password","comment_id","comment_text"] of
            [Just usIdParam,Just pwdParam,Just commentIdParam,Just commentTextParam] -> do
              pwd <- selectFromDb1 conn "users" ("user_id",usIdParam) "password"
              case pwd == pwdParam of
                True -> do
                  usCommentId <- (selectFromDb1 conn "comments" ("comment_id",commentIdParam) "user_id") :: IO Integer
                  case usCommentId == (read . unpack $ usIdParam) of
                    True -> do
                      execute conn "DELETE FROM comments WHERE comment_id = ?" [commentIdParam]
                      postId  <- (selectFromDb1 conn "comments" ("comment_id",commentIdParam) "post_id") :: IO Integer
                      okHelper $ lazyByteString $ encode $ CommentResponse { comment_id = (read . unpack $ commentIdParam) , comment_text = commentTextParam, post_id6 = postId, user_id6 = usCommentId}
                    _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "User cannot update this comment"}
                _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "INVALID password"}
            _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t parse query parameter"}
    ["deleteComment"]  -> do
      case fmap (isExistParam req) ["user_id","admin_id","password","comment_id"] of
        [True,True,True,True] -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Too much query parameters"}
        [True,False,True,True] -> do
          case fmap (parseParam req) ["user_id","password","comment_id"] of
            [Just usIdParam,Just pwdParam,Just commentIdParam] -> do
              pwd <- selectFromDb1 conn "users" ("user_id",usIdParam) "password"
              case pwd == pwdParam of
                True -> do
                  usCommentId <- (selectFromDb1 conn "comments" ("comment_id",commentIdParam) "user_id") :: IO Integer
                  postId      <- (selectFromDb1 conn "comments" ("comment_id",commentIdParam) "post_id") :: IO Integer
                  authPostId  <- (selectFromDb1 conn "posts" ("post_id",pack . show $ postId) "author_id") :: IO Integer
                  usPostId  <- (selectFromDb1 conn "authors" ("author_id",pack . show $ authPostId) "user_id") :: IO Integer
                  case (usCommentId == (read . unpack $ usIdParam)) || (usPostId == (read . unpack $ usIdParam)) of
                    True -> do
                      execute conn "DELETE FROM comments WHERE comment_id = ?" [commentIdParam]
                      okHelper $ lazyByteString $ encode (OkResponse { ok = True })
                    _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "User cannot delete this comment"}
                _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "INVALID password"}
            _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t parse query parameter"}
        [False,True,True,True] -> do
          case fmap (parseParam req) ["admin_id","password","comment_id"] of
            [Just adminIdParam,Just pwdParam,Just commentIdParam] -> do
              [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
              [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
              case zoo pwdParam pwd admBool of
                "Success" -> do
                  execute conn "DELETE FROM comments WHERE comment_id = ?" [commentIdParam]
                  okHelper $ lazyByteString $ encode (OkResponse { ok = True })
                "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
                "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
                "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
            _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t parse query parameter"}
        _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t find query parameter"}
    ["picture",picId]  -> do
      picUrl <- selectFromDb1 conn "pics" ("pic_id",picId) "pic_url"
      res <- httpLBS $ fromString $ unpack $ picUrl
      send $ responseBuilder 
        status200 
        [("Content-Type", "image/jpeg")] 
        $ lazyByteString $ getResponseBody res 


chooseArgs req =
  let filterDateList   = ["created_at","created_at_lt","created_at_gt"] in
  let filterTagList    = ["tag","tags_in","tags_all"] in
  let filterInList     = ["name_in","text_in","everywhere_in"] in
  let filterParamsList = filterDateList ++ ["category_id","author_name"] ++ filterTagList ++ filterInList in
  let sortList         = ["sort_by_pics_number","sort_by_category","sort_by_author","sort_by_date"] in
  case fmap (checkComb req) [filterDateList,filterTagList,filterInList] of
    [True,True,True] -> case ( sequence . concatMap (checkFilterParam req) $ filterParamsList, sequence . concatMap (checkSortParam req) $ sortList) of
      (Right filterArgs, Right sortArgs) -> if not . isDateASC $ sortArgs 
        then 
          let table     = intercalate " " $ ["posts JOIN authors ON authors.author_id = posts.author_id"] ++ (firstThree . unzip3 $ filterArgs) ++ (firstThree . unzip3 $ sortArgs) 
              where'    = intercalate " AND " $ (secondThree . unzip3 $ filterArgs) ++ ["true"]
              orderBy   = intercalate "," $ (secondThree . unzip3 $ sortArgs) ++ ["post_create_date DESC, post_id DESC"]
              values    = (Prelude.concat . fmap fst . thirdThree . unzip3 $ filterArgs) ++  (Prelude.concat . fmap snd . thirdThree . unzip3 $ filterArgs)
          in Right (table,where',orderBy,values)
        else
          let table     = intercalate " " $ ["posts JOIN authors ON authors.author_id = posts.author_id"] ++ (firstThree . unzip3 $ filterArgs) ++ (firstThree . unzip3 $ sortArgs)  
              where'    = intercalate " AND " $ (secondThree . unzip3 $ filterArgs) ++ ["true"]
              orderBy   = intercalate "," $ (secondThree . unzip3 $ sortArgs) ++ ["post_create_date ASC, post_id ASC"]
              values    = (Prelude.concat . fmap fst . thirdThree . unzip3 $ filterArgs) ++  (Prelude.concat . fmap snd . thirdThree . unzip3 $ filterArgs)
          in Right (table,where',orderBy,values)
      (Left a,_) -> Left a
      (_,Left b) -> Left b

isDateASC xs = foldr (\(a,b,c) cont -> if c == DateASC then True else cont) False xs


checkComb req list =
   case fmap (isExistParam req) list of
     (True:True:_)   -> False
     (_:True:True:_) -> False
     (True:_:True:_) -> False
     _               -> True

--Left "Invalid combination of parameters"
--"posts JOIN authors ON authors.author_id = posts.author_id"
--", post_create_date DESC, post_id DESC"

checkFilterParam :: Request -> Text -> [Either String (String,String,([Text],[Text]))]
checkFilterParam req param =
  case isExistParam req param of
    False -> []
    True  -> case parseParam req param of
      Just txt -> chooseFilterArgs txt param
      _ -> [Left $ "Can`t parse query parameter" ++ unpack param]

chooseFilterArgs x param = case param of
  "created_at" -> 
          let table   = ""
              where'  = "post_create_date = ?"
              values  = ([],[x])
          in [Right (table,where',values)]
  "created_at_lt" ->
          let table   = ""
              where'  = "post_create_date < ?"
              values  = ([],[x])
          in [Right (table,where',values)]
  "created_at_gt" ->
          let table   = ""
              where'  = "post_create_date < ?"
              values  = ([],[x])
          in [Right (table,where',values)]
  "category_id" ->
          let table   = ""
              where'  = "post_category_id = ?"
              values  = ([],[x])
          in [Right (table,where',values)]
  "tag" ->
          let table   = "JOIN (SELECT post_id FROM poststags WHERE tag_id = ? GROUP BY post_id) AS t ON posts.post_id=t.post_id"
              where'  = "true"
              values  = ([x],[])
          in [Right (table,where',values)]
  "tags_in" ->
          let table   = "JOIN (SELECT post_id FROM poststags WHERE tag_id IN (" ++ (init . tail . unpack $ x) ++ ") GROUP BY post_id) AS t ON posts.post_id=t.post_id"
              where'  = "true"
              values  = ([],[])
          in [Right (table,where',values)]
  "tags_all" ->
          let table   = "JOIN (SELECT post_id, array_agg(ARRAY[tag_id]) AS tags_id FROM poststags GROUP BY post_id) AS t ON posts.post_id=t.post_id"
              where'  = "tags_id @> ARRAY" ++ unpack x ++ "::bigint[]"
              values  = ([],[])
          in [Right (table,where',values)]
  "name_in" ->
          let table   = ""
              where'  = "post_name ILIKE ?"
              values  = ([],[Data.Text.concat ["%",x,"%"]])          
          in [Right (table,where',values)]
  "text_in" ->
          let table   = ""
              where'  = "post_text ILIKE ?"
              values  = ([],[Data.Text.concat ["%",x,"%"]])          
          in [Right (table,where',values)]
  "everywhere_in" ->
          let table   = "JOIN categories AS c ON c.category_id=posts.post_category_id JOIN (SELECT pt.post_id, bool_or(tag_name ILIKE ? ) AS isintag FROM poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id  GROUP BY pt.post_id) AS tg ON tg.post_id=posts.post_id"
              where'  = "(post_text ILIKE ? OR post_name ILIKE ? OR c.category_name ILIKE ? OR isintag = TRUE)"
              values  = ([Data.Text.concat ["%",x,"%"]],replicate 3 $ Data.Text.concat ["%",x,"%"])
          in [Right (table,where',values)]
  "author_name" ->
          let table   = "JOIN users AS us ON authors.user_id=us.user_id"
              where'  = "us.first_name = ?"
              values  = ([],[x])
          in [Right (table,where',values)]     
  _ -> [Left $ "Can`t parse query parameter" ++ unpack param]
        

checkSortParam :: Request -> Text -> [Either String (String,String,SortDate)] 
checkSortParam req param =
  case isExistParam req param of
    False -> []
    True  -> case parseParam req param of
      Just txt -> chooseSortArgs txt param
      _ -> [Left $ "Can`t parse query parameter" ++ unpack param]

chooseSortArgs "DESC" param =
  case param of
        "sort_by_pics_number" -> 
          let joinTable   = "JOIN (SELECT post_id, count (post_id) AS count_pics FROM postspics GROUP BY post_id) AS counts ON posts.post_id=counts.post_id"
              orderBy = "count_pics DESC"
          in [Right (joinTable,orderBy,defDateSort)]
        "sort_by_category" ->
          let joinTable   = "JOIN categories ON posts.post_category_id=categories.category_id"
              orderBy = "category_name DESC"
          in [Right (joinTable,orderBy,defDateSort)]
        "sort_by_author" ->
          let joinTable   = "JOIN users AS u ON authors.user_id=u.user_id"
              orderBy = "u.first_name DESC"
          in [Right (joinTable,orderBy,defDateSort)]
        "sort_by_date" ->
          let joinTable   = ""
              orderBy = "true"
          in [Right (joinTable,orderBy,DateDESC)]
chooseSortArgs "ASC" param =
  case param of
        "sort_by_pics_number" -> 
          let joinTable   = "JOIN (SELECT post_id, count (post_id) AS count_pics FROM postspics GROUP BY post_id) AS counts ON posts.post_id=counts.post_id"
              orderBy = "count_pics ASC"
          in [Right (joinTable,orderBy,defDateSort)]
        "sort_by_category" ->
          let joinTable   = "JOIN categories ON posts.post_category_id=categories.category_id"
              orderBy = "category_name ASC"
          in [Right (joinTable,orderBy,defDateSort)]
        "sort_by_author" ->
          let joinTable   = "JOIN users AS u ON authors.user_id=u.user_id"
              orderBy = "u.first_name ASC"
          in [Right (joinTable,orderBy,defDateSort)]
        "sort_by_date" ->
          let joinTable   = ""
              orderBy = "true"
          in [Right (joinTable,orderBy,DateASC)]
chooseSortArgs txt param 
  | Data.Text.toUpper txt == "ASC"  = chooseSortArgs "ASC"  param
  | Data.Text.toUpper txt == "DESC" = chooseSortArgs "DESC" param
  | otherwise                       = [Left $ "Invalid sort parameter" ++ unpack param]


data SortDate = DateASC | DateDESC 
 deriving (Eq,Show,Read)

defDateSort = DateDESC

                                                                             

isExistParam req txt = case lookup txt $ queryToQueryText $ queryString req of
  Just _  -> True
  Nothing -> False

data ReqError = SecretError | SimpleError String
  deriving Show

isExistParamE :: (Monad m) => Request -> Text -> ExceptT ReqError m (Maybe Text)
isExistParamE req param = case lookup param $ queryToQueryText $ queryString req of
  Just x  -> return x
  Nothing -> throwE $ SimpleError $ "Can't find param" ++ unpack param

parseParam req txt = fromJust . lookup txt $ queryToQueryText $ queryString req

parseParamE req param = case fromJust . lookup param $ queryToQueryText $ queryString req of
  Just x  -> Right x
  Nothing -> Left $ SimpleError $ "Can't parse param" ++ unpack param




--selectFromDb1 :: (Database.PostgreSQL.Simple.FromField.FromField a) => Connection -> String -> (String,Text) -> String -> IO a
selectFromDb1 conn table (eqParamName,eqParamValue) param = do
  [Only value] <- query conn (fromString $ "SELECT " ++ param ++ " FROM " ++ table ++ " WHERE " ++ eqParamName ++ " = ?") [eqParamValue]
  return value

selectManyWhereFromDb conn table (eqParamName,eqParamValue) params = do
  xs <- query conn (fromString $ "SELECT " ++ (intercalate ", " params) ++ " FROM " ++ table ++ " WHERE " ++ eqParamName ++ " = ?") [eqParamValue]
  return xs

selectTupleFromDb1 conn table params = do
  xs <- query_ conn (fromString $ "SELECT " ++ (intercalate ", " params) ++ " FROM " ++ table) 
  return xs

selectManyLimitWhereFromDb conn table (eqParamName,eqParamValue) params page limitNumber = do
  xs <- query conn (fromString $ "SELECT " ++ (intercalate ", " params) ++ " FROM " ++ table ++ " WHERE " ++ eqParamName ++ " = ? OFFSET " ++ show ((page-1)*limitNumber) ++ " LIMIT " ++ show (page*limitNumber)) [eqParamValue]
  return xs


selectManyLimitFromDb conn table page limitNumber params   = do
  xs <- query_ conn (fromString $ "SELECT " ++ (intercalate ", " params) ++ " FROM " ++ table ++ " OFFSET " ++ show ((page -1)*limitNumber) ++ " LIMIT " ++ show (page*limitNumber)) 
  return xs

selectManyOrderLimitFromDb conn table orderBy page limitNumber params   = do
  xs <- query_ conn (fromString $ "SELECT " ++ (intercalate ", " params) ++ " FROM " ++ table ++ " ORDER BY " ++ orderBy ++ " OFFSET " ++ show ((page -1)*limitNumber) ++ " LIMIT " ++ show (page*limitNumber)) 
  return xs

selectManyOrderLimitWhereFromDb conn table orderBy page limitNumber params where' values = do
  xs <- query conn (fromString $ "SELECT " ++ (intercalate ", " params) ++ " FROM " ++ table ++ " WHERE " ++ where' ++ " ORDER BY " ++ orderBy ++ " OFFSET " ++ show ((page -1)*limitNumber) ++ " LIMIT " ++ show (page*limitNumber) ) values
  return xs

reverseselectFromDb1 conn table param eqParamName eqParamValue = selectFromDb1 conn table (eqParamName,eqParamValue) param

--selectListFromDb :: (Database.PostgreSQL.Simple.FromField.FromField a) => Connection -> String -> (String,Text) -> String -> IO [a]
selectListFromDb conn table (eqParamName,eqParamValue) param  = do
  xs <- query conn (fromString $ "SELECT " ++ param ++ " FROM " ++ table ++ " WHERE " ++ eqParamName ++ " = ?") [eqParamValue]
  return (fmap fromOnly xs)

reverseSelectListFromDb conn table param eqParamName eqParamValue = selectListFromDb conn table (eqParamName,eqParamValue) param


--selectLimitListFromDb :: (Database.PostgreSQL.Simple.FromField.FromField a) =>  Connection -> String -> (String,Text) -> String -> Integer -> Integer -> IO [a]
selectLimitListFromDb conn table (eqParamName,eqParamValue) param page limitNumber = do
  xs <- query conn (fromString $ "SELECT " ++ param ++ " FROM " ++ table ++ " WHERE " ++ eqParamName ++ " = ? OFFSET " ++ show ((page-1)*limitNumber) ++ " LIMIT " ++ show (page*limitNumber)) [eqParamValue]
  return (fmap fromOnly xs)  

--updateDb :: Connection -> String -> (String,Text) -> String -> Text -> IO GHC.Int.Int64
updateDb conn table (setName,setValue) eqParamName eqParamValue  = do
  execute conn (fromString $ "UPDATE " ++ table ++ " SET " ++ setName ++ " = ? WHERE " ++ eqParamName ++ " = ?") [setValue,eqParamValue]
  


--deleteFromDb1 :: Connection -> String -> String -> Text -> IO GHC.Int.Int64
deleteFromDb1 conn table eqParamName eqParamValue = do
  execute conn (fromString $ "DELETE FROM " ++ table ++ " WHERE " ++ eqParamName ++ " = ?") [eqParamValue]
  

--coo [a] [b] [c] [d] = [CommentIdTextUserResponse a b c d]
--coo (a:as) (b:bs) (c:cs) (d:ds) = (CommentIdTextUserResponse a b c d) : coo as bs cs ds
--coo :: [(Integer,Text,Integer)] -> [CommentIdTextUserResponse]
coo [(a,b,c)] = [CommentIdTextUserResponse a b c]
coo ((a,b,c):cs) = (CommentIdTextUserResponse a b c): coo cs

findAllSubCat :: Integer -> IO [Integer]
findAllSubCat catId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  [Only tf] <- query conn "SELECT EXISTS (SELECT category_id FROM categories WHERE super_category_id = ?)" [pack . show $ catId]
  case tf of
    False -> return [catId]
    True  -> do
      xs <- selectListFromDb conn "categories" ("super_category_id",(pack . show $ catId)) "category_id"
      ys <- mapM findAllSubCat xs
      return $ catId : (Prelude.concat  ys)


adminAuth pwdParam pwd admBool
  | admBool && (pwd == pwdParam) = return True
  | admBool                      = throwE . SimpleError $ "INVALID pwd, admin = True "
  | (pwd == pwdParam)            = throwE . SimpleError $ "valid pwd, user is NOT admin"
  | otherwise                    = throwE . SimpleError $ "INVALID pwd, user is NOT admin"

zoo pwdParam pwd admBool 
  | admBool && (pwd == pwdParam) = "Success"
  | admBool                      = "INVALID pwd, admin = True "
  | (pwd == pwdParam)            = "valid pwd, user is NOT admin"
  | otherwise                    = "INVALID pwd, user is NOT admin"

boo draftId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute conn "DELETE FROM draftspics WHERE draft_id = ?" [pack . show $ draftId]

doo draftId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute conn "DELETE FROM draftstags WHERE draft_id = ?" [pack . show $ draftId]

hoo draftId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute conn "DELETE FROM drafts WHERE draft_id = ?" [pack . show $ draftId]   

voo picId = pack $ "http://localhost:3000/picture/" ++ show picId

koo draftId tagId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute conn "INSERT INTO draftstags (draft_id , tag_id) VALUES (?,?)" [pack . show $ draftId, pack . show $ tagId]

woo postId tagId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute conn "INSERT INTO poststags (post_id , tag_id) VALUES (?,?)" [pack . show $ postId, pack . show $ tagId]

poo draftId picId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute conn "INSERT INTO draftspics (draft_id , pic_id) VALUES (?,?)" [pack . show $ draftId, pack . show $ picId]

qoo postId picId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute conn "INSERT INTO postspics (post_id , pic_id) VALUES (?,?)" [pack . show $ postId, pack . show $ picId]


roo tagId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  [Only tagName] <- query conn "SELECT tag_name FROM tags WHERE tag_id = ?" [pack . show $ tagId]
  return $ TagResponse tagId tagName

goo :: Text -> IO Integer
goo t = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  [Only picId] <- query conn "INSERT INTO pics (pic_url) VALUES (?) RETURNING pic_id" [t]
  return picId

loo [x] [y] = [PicIdUrl x y]
loo (x:xs) (y:ys) = PicIdUrl x y : loo xs ys



foo :: Integer -> IO [(Integer,Text)]
foo catId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  [Only catName] <- query conn "SELECT category_name FROM categories WHERE category_id = ? " [pack . show $ catId]
  [Only superCatId]   <- query conn "SELECT COALESCE (super_category_id, '0') AS super_category_id FROM categories WHERE category_id = ?" [pack . show $ catId]
  case superCatId of 
    0 -> return $ [(catId,catName)]
    _ -> do
      xs <- foo superCatId
      return $ ((catId,catName) : xs) 

inCatResp [(x,y)] = CatResponse { cat_id = x , cat_name =  y , super_cat = "NULL"}
inCatResp ((x,y):xs) = SubCatResponse { subCat_id = x , subCat_name =  y , super_category = inCatResp xs}

prettyNull :: String -> String
prettyNull "0" = "NULL" 
prettyNull a   = a       
      
      
main :: IO ()
main = do
  createDbStructure
  addDefaultParameters
  run 3000 application




firstThree (a,b,c) = a
firstFour  (a,b,c,d) = a
firstFive  (a,b,c,d,e) = a
firstSix   (a,b,c,d,e,f) = a
firstSeven (a,b,c,d,e,f,g) = a
firstEight (a,b,c,d,e,f,g,h) = a
firstNine  (a,b,c,d,e,f,g,h,i) = a
firstTen   (a,b,c,d,e,f,g,h,i,j) = a

secondThree (a,b,c) = b
secondFour  (a,b,c,d) = b
secondFive  (a,b,c,d,e) = b
secondSix   (a,b,c,d,e,f) = b
secondSeven (a,b,c,d,e,f,g) = b
secondEight (a,b,c,d,e,f,g,h) = b
secondNine  (a,b,c,d,e,f,g,h,i) = b
secondTen   (a,b,c,d,e,f,g,h,i,j) = b

thirdThree (a,b,c) = c
thirdFour  (a,b,c,d) = c
thirdFive  (a,b,c,d,e) = c
thirdSix   (a,b,c,d,e,f) = c
thirdSeven (a,b,c,d,e,f,g) = c
thirdEight (a,b,c,d,e,f,g,h) = c
thirdNine  (a,b,c,d,e,f,g,h,i) = c
thirdTen   (a,b,c,d,e,f,g,h,i,j) = c

fourthFour  (a,b,c,d) = d
fourthFive  (a,b,c,d,e) = d
fourthSix   (a,b,c,d,e,f) = d
fourthSeven (a,b,c,d,e,f,g) = d
fourthEight (a,b,c,d,e,f,g,h) = d
fourthNine  (a,b,c,d,e,f,g,h,i) = d
fourthTen   (a,b,c,d,e,f,g,h,i,j) = d

fifthFive  (a,b,c,d,e) = e
fifthSix   (a,b,c,d,e,f) = e
fifthSeven (a,b,c,d,e,f,g) = e
fifthEight (a,b,c,d,e,f,g,h) = e
fifthNine  (a,b,c,d,e,f,g,h,i) = e
fifthTen   (a,b,c,d,e,f,g,h,i,j) = e

sixthSix   (a,b,c,d,e,f) = f
sixthSeven (a,b,c,d,e,f,g) = f
sixthEight (a,b,c,d,e,f,g,h) = f
sixthNine  (a,b,c,d,e,f,g,h,i) = f
sixthTen   (a,b,c,d,e,f,g,h,i,j) = f

seventhSeven (a,b,c,d,e,f,g) = g
seventhEight (a,b,c,d,e,f,g,h) = g
seventhNine  (a,b,c,d,e,f,g,h,i) = g
seventhTen   (a,b,c,d,e,f,g,h,i,j) = g

eighthEight (a,b,c,d,e,f,g,h) = h
eighthNine  (a,b,c,d,e,f,g,h,i) = h
eighthTen   (a,b,c,d,e,f,g,h,i,j) = h

ninthNine  (a,b,c,d,e,f,g,h,i) = i
ninthTen   (a,b,c,d,e,f,g,h,i,j) = i