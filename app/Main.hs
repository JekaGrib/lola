{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Simple            ( parseRequest, setRequestBody, getResponseBody, httpLBS )
import           Network.Wai
import           Network.HTTP.Types             ( status200, status404, status301, movedPermanently301 )
import           Network.HTTP.Types.URI         ( queryToQueryText )
import           Network.Wai.Handler.Warp       ( run )
import           Data.Aeson
import           Data.Text
import           Data.ByteString.Builder        ( lazyByteString )
import           Database.PostgreSQL.Simple
import           Network.HTTP.Simple            ( parseRequest, setRequestBody, getResponseBody, httpLBS )
import           Data.Maybe                     ( fromJust )
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Calendar             ( showGregorian, Day )
import           Database.PostgreSQL.Simple.Time

getOrdinalDay :: IO String
getOrdinalDay = do
  time    <- getZonedTime
  let day = showOrdinalDate . localDay . zonedTimeToLocalTime $ time
  return day

getGregorianDay :: IO String
getGregorianDay = do
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
  execute_ conn "CREATE TABLE authors ( author_id BIGSERIAL PRIMARY KEY NOT NULL, author_info VARCHAR(1000) NOT NULL, user_id BIGINT NOT NULL REFERENCES users(user_id))"
  print "kk"

createPicsTable :: IO ()
createPicsTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE pics ( pic_id BIGSERIAL PRIMARY KEY NOT NULL, pic_url VARCHAR(1000) NOT NULL)"
  print "kk"

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

addKey = do
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
  addKey

data CreateUserResponse = CreateUserResponse {
      user_id      :: Integer
    , first_name   :: Text
    , last_name    :: Text
    , user_pic_id  :: Integer
    , user_pic_url :: Text
    } deriving Show

instance ToJSON CreateUserResponse where
    toJSON (CreateUserResponse user_id first_name last_name user_pic_id user_pic_url) =
        object ["user_id" .= user_id, "first_name" .= first_name, "last_name" .= last_name, "user_pic_id" .= user_pic_id, "user_pic_url" .= user_pic_url]
    toEncoding (CreateUserResponse user_id first_name last_name user_pic_id user_pic_url) =
        pairs ("user_id" .= user_id <> "first_name" .= first_name <> "last_name" .= last_name <> "user_pic_id" .= user_pic_id <> "user_pic_url" .= user_pic_url)

data DeleteUserResponse = DeleteUserResponse {ok :: Bool}

instance ToJSON DeleteUserResponse where
    toJSON (DeleteUserResponse ok) =
        object ["ok" .= ok]
    toEncoding (DeleteUserResponse ok) =
        pairs ("ok" .= ok )

data CreateAuthorResponse = CreateAuthorResponse {
      author_id    :: Integer
    , auth_user_id :: Integer
    , author_info  :: Text
    } deriving Show

instance ToJSON CreateAuthorResponse where
    toJSON (CreateAuthorResponse author_id auth_user_id author_info ) =
        object ["author_id" .= author_id, "user_id" .= auth_user_id, "author_info" .= author_info]
    toEncoding (CreateAuthorResponse author_id auth_user_id author_info ) =
        pairs ( "author_id" .= author_id <> "user_id" .= auth_user_id <> "author_info" .= author_info)

data CreateCatResponse 
  = CreateSubCatResponse {
      subCat_id      :: Integer
    , subCat_name    :: Text
    , super_category :: CreateCatResponse
    } 
  | CreateCatResponse {
      cat_id    :: Integer
    , cat_name  :: Text
    , super_cat :: Text
    } deriving Show


instance ToJSON CreateCatResponse where
    toJSON (CreateCatResponse cat_id cat_name super_cat) =
        object ["category_id" .= cat_id, "category_name" .= cat_name, "super_category" .= super_cat]
    toJSON (CreateSubCatResponse cat_id cat_name super_cat) =
        object ["category_id" .= cat_id, "category_name" .= cat_name, "super_category" .= super_cat]
    toEncoding (CreateCatResponse cat_id cat_name super_cat) =
        pairs ( "category_id" .= cat_id <> "category_name" .= cat_name <> "super_category" .= super_cat)
    toEncoding (CreateSubCatResponse cat_id cat_name super_cat) =
        pairs ( "category_id" .= cat_id <> "category_name" .= cat_name <> "super_category" .= super_cat)

data CreateDraftRequest = CreateDraftRequest {
      user_id1      :: Integer
    , password1   :: Text
    , draft_name    :: Text
    , draft_cat_id :: Integer
    , draft_text1  :: Text
    , draft_main_pic_url :: Text
    , draft_pics_urls :: [PicUrl]
    , draft_tags_ids :: [TagId]
    } deriving Show

instance FromJSON CreateDraftRequest where
    parseJSON (Object v) = CreateDraftRequest
        <$> v .: "user_id"
        <*> v .: "password"
        <*> v .: "draft_name"
        <*> v .: "draft_category_id"
        <*> v .: "draft_text"
        <*> v .: "draft_main_pic_url"
        <*> v .: "draft_pics_urls"
        <*> v .: "draft_tags_ids" 

instance ToJSON CreateDraftRequest where
    toJSON (CreateDraftRequest user_id1 password1 draft_name draft_cat_id draft_text1 draft_main_pic_url draft_pics_urls draft_tags_ids ) =
        object ["user_id" .= user_id1, "password" .= password1, "draft_name" .= draft_name, "draft_category_id" .= draft_cat_id, "draft_text" .= draft_text1, "draft_main_pic_url" .= draft_main_pic_url, "draft_pics_urls" .= draft_pics_urls, "draft_tags_ids" .= draft_tags_ids]
    toEncoding (CreateDraftRequest user_id1 password1 draft_name draft_cat_id draft_text1 draft_main_pic_url draft_pics_urls draft_tags_ids) =
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


data CreateNewDraftResponse = CreateNewDraftResponse {
      draft_id      :: Integer
    , post_id2      :: Text
    , author_id2   :: Integer
    , draft_name2    :: Text
    , draft_cat :: CreateCatResponse
    , draft_text2  :: Text
    , draft_main_pic_id  :: Integer
    , draft_main_pic_url2 :: Text
    , draft_pics2 :: [PicIdUrl]
    , draft_tags ::  [CreateTagResponse]
    } deriving Show

instance ToJSON CreateNewDraftResponse where
    toJSON (CreateNewDraftResponse draft_id post_id2 author_id2 draft_name2 draft_cat draft_text2 draft_main_pic_id draft_main_pic_url2 draft_pics2 draft_tags) =
        object ["draft_id" .= draft_id, "post_id" .= post_id2, "author_id" .= author_id2, "draft_name" .= draft_name2, "draft_category" .= draft_cat, "draft_text" .= draft_text2, "draft_main_pic_id" .= draft_main_pic_id, "draft_main_pic_url" .= draft_main_pic_url2, "draft_pics" .= draft_pics2, "draft_tags" .= draft_tags]
    toEncoding (CreateNewDraftResponse draft_id post_id2 author_id2 draft_name2 draft_cat draft_text2 draft_main_pic_id draft_main_pic_url2 draft_pics2 draft_tags ) =
        pairs ("draft_id" .= draft_id <> "post_id" .= post_id2 <> "author_id" .= author_id2 <> "draft_name" .= draft_name2 <> "draft_category" .= draft_cat <> "draft_text" .= draft_text2 <> "draft_main_pic_id" .= draft_main_pic_id <> "draft_main_pic_url" .= draft_main_pic_url2 <> "draft_pics" .= draft_pics2 <> "draft_tags" .= draft_tags)


data PicIdUrl = PicIdUrl {
      pic_id   :: Integer
    , pic_url2 :: Text
    } deriving Show

instance ToJSON PicIdUrl where
    toJSON (PicIdUrl pic_id2 pic_url ) =
        object ["pic_id" .= pic_id2, "pic_url" .= pic_url]
    toEncoding (PicIdUrl pic_id2 pic_url ) =
        pairs ( "pic_id" .= pic_id2 <> "pic_url" .= pic_url )

data CreatePostsDraftResponse = CreatePostsDraftResponse {
      draft_id5      :: Integer
    , post_id5      :: Integer
    , author_id5   :: Integer
    , draft_name5    :: Text
    , draft_cat5 :: CreateCatResponse
    , draft_text5  :: Text
    , draft_main_pic_id5  :: Integer
    , draft_main_pic_url5 :: Text
    , draft_pics5 :: [PicIdUrl]
    , draft_tags5 ::  [CreateTagResponse]
    } deriving Show

instance ToJSON CreatePostsDraftResponse where
    toJSON (CreatePostsDraftResponse draft_id5 post_id5 author_id5 draft_name5 draft_cat5 draft_text5 draft_main_pic_id5 draft_main_pic_url5 draft_pics5 draft_tags5) =
        object ["draft_id" .= draft_id5, "post_id" .= post_id5, "author_id" .= author_id5, "draft_name" .= draft_name5, "draft_category" .= draft_cat5, "draft_text" .= draft_text5, "draft_main_pic_id" .= draft_main_pic_id5, "draft_main_pic_url" .= draft_main_pic_url5, "draft_pics" .= draft_pics5, "draft_tags" .= draft_tags5]
    toEncoding (CreatePostsDraftResponse draft_id5 post_id5 author_id5 draft_name5 draft_cat5 draft_text5 draft_main_pic_id5 draft_main_pic_url5 draft_pics5 draft_tags5 ) =
        pairs ("draft_id" .= draft_id5 <> "post_id" .= post_id5 <> "author_id" .= author_id5 <> "draft_name" .= draft_name5 <> "draft_category" .= draft_cat5 <> "draft_text" .= draft_text5 <> "draft_main_pic_id" .= draft_main_pic_id5 <> "draft_main_pic_url" .= draft_main_pic_url5 <> "draft_pics" .= draft_pics5 <> "draft_tags" .= draft_tags5)

data PublishDraftResponse = PublishDraftResponse {
      post_id      :: Integer
    , author_id4   :: Integer
    , post_name    :: Text
    , post_create_date :: Text
    , post_cat     :: CreateCatResponse
    , post_text    :: Text
    , post_main_pic_id  :: Integer
    , post_main_pic_url :: Text
    , post_pics :: [PicIdUrl]
    , post_tags :: [CreateTagResponse]
    } deriving Show

instance ToJSON PublishDraftResponse where
    toJSON (PublishDraftResponse post_id author_id4 post_name post_create_date post_cat post_text post_main_pic_id post_main_pic_url post_pics post_tags) =
        object ["post_id" .= post_id, "author_id" .= author_id4, "post_name" .= post_name, "post_create_date" .= post_create_date, "post_category" .= post_cat, "post_text" .= post_text, "post_main_pic_id" .= post_main_pic_id, "post_main_pic_url" .= post_main_pic_url, "post_pics" .= post_pics, "post_tags" .= post_tags]
    toEncoding (PublishDraftResponse post_id author_id4 post_name post_create_date post_cat post_text post_main_pic_id post_main_pic_url post_pics post_tags) =
        pairs ("post_id" .= post_id <> "author_id" .= author_id4  <> "post_name" .= post_name <> "post_create_date" .= post_create_date <> "post_category" .= post_cat <> "post_text" .= post_text <> "post_main_pic_id" .= post_main_pic_id <> "post_main_pic_url" .= post_main_pic_url <> "post_pics" .= post_pics <> "post_tags" .= post_tags)



data CreateTagResponse = CreateTagResponse {
      tag_id   :: Integer
    , tag_name :: Text
    } deriving Show

instance ToJSON CreateTagResponse where
    toJSON (CreateTagResponse tag_id tag_name ) =
        object ["tag_id" .= tag_id, "tag_name" .= tag_name]
    toEncoding (CreateTagResponse tag_id tag_name ) =
        pairs ( "tag_id" .= tag_id <> "tag_name" .= tag_name )

data CreateCommentResponse = CreateCommentResponse {
      comment_id   :: Integer
    , comment_text :: Text
    , post_id6   :: Integer
    , user_id6   :: Integer
    } deriving Show

instance ToJSON CreateCommentResponse where
    toJSON (CreateCommentResponse comment_id comment_text post_id user_id) =
        object ["comment_id" .= comment_id, "comment_text" .= comment_text, "post_id" .= post_id, "user_id" .= user_id]
    toEncoding (CreateCommentResponse comment_id comment_text post_id user_id) =
        pairs ( "comment_id" .= comment_id <> "comment_text" .= comment_text <> "post_id" .= post_id <> "user_id" .= user_id )

{-

data CreateSubCatResponse = CreateSubCatResponse {
      subCat_id    :: Integer
    , subCat_name  :: Text
    , super_cat_id :: Integer
    } deriving Show

instance ToJSON CreateSubCatResponse where
    toJSON (CreateSubCatResponse subCat_id subCat_name super_cat_id) =
        object ["category_id" .= subCat_id, "category_name" .= subCat_name, "super_category_id" .= super_cat_id]
    toEncoding (CreateSubCatResponse subCat_id subCat_name super_cat_id) =
        pairs ( "category_id" .= subCat_id <> "category_name" .= subCat_name <> "super_category_id" .= super_cat_id)

-}

application req send = do
  let okHelper = send . responseBuilder status200 [("Content-Type", "application/json; charset=utf-8")]
  case pathInfo req of
    ["createUser"]        -> do
      let passwordParam   = fromJust . fromJust . lookup     "password" $ queryToQueryText $ queryString req
      let firstNameParam  = fromJust . fromJust . lookup   "first_name" $ queryToQueryText $ queryString req
      let lastNameParam   = fromJust . fromJust . lookup    "last_name" $ queryToQueryText $ queryString req
      let userPicUrlParam = fromJust . fromJust . lookup "user_pic_url" $ queryToQueryText $ queryString req
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only picId]  <- query conn "INSERT INTO pics ( pic_url ) VALUES (?) RETURNING pic_id" [userPicUrlParam]
      day <- getOrdinalDay  
      [Only userId] <- query conn "INSERT INTO users ( password, first_name , last_name , user_pic_id , user_create_date, admin) VALUES ( ?,?,?,?,?, false ) RETURNING user_id" [ passwordParam , firstNameParam, lastNameParam, pack (show picId), pack day ]
      okHelper $ lazyByteString $ encode (CreateUserResponse {user_id = userId , first_name = firstNameParam , last_name = lastNameParam, user_pic_id = picId, user_pic_url = pack ("http://localhost:3000/pic_" ++ show picId)})
    ["getUser", userId]      -> do
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only firstName] <- query conn "SELECT first_name FROM users WHERE user_id = ? " [userId ]
      [Only lastName] <- query conn "SELECT last_name FROM users WHERE user_id = ? " [userId]
      [Only picId] <- query conn "SELECT user_pic_id FROM users WHERE user_id = ? " [userId]
      okHelper $ lazyByteString $ encode (CreateUserResponse {user_id = read (unpack(userId)) , first_name = firstName , last_name = lastName, user_pic_id = picId, user_pic_url = pack ("http://localhost:3000/pic_" ++ show picId)})
    ["deleteUser"]   -> do
      let userIdParam   = fromJust . fromJust . lookup "user_id"  $ queryToQueryText $ queryString req
      let passwordParam = fromJust . fromJust . lookup "password" $ queryToQueryText $ queryString req
      let adminIdParam  = fromJust . fromJust . lookup "admin_id" $ queryToQueryText $ queryString req
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only admPassword] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case admBool of
        True -> do
          case (unpack $ passwordParam) == admPassword of
            True -> do
              execute conn "DELETE FROM users WHERE user_id = ? " [userIdParam]
              okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = True})
            False -> do
              okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = False})
        False ->  send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["createAdmin"]        -> do
      let createAdminKey  = fromJust . fromJust . lookup "create_admin_key" $ queryToQueryText $ queryString req
      let passwordParam   = fromJust . fromJust . lookup         "password" $ queryToQueryText $ queryString req
      let firstNameParam  = fromJust . fromJust . lookup       "first_name" $ queryToQueryText $ queryString req
      let lastNameParam   = fromJust . fromJust . lookup        "last_name" $ queryToQueryText $ queryString req
      let userPicUrlParam = fromJust . fromJust . lookup     "user_pic_url" $ queryToQueryText $ queryString req
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only key]  <- query_ conn "SELECT create_admin_key FROM key"
      case  createAdminKey == key of
        True -> do
          [Only picId]  <- query conn "INSERT INTO pics ( pic_url ) VALUES (?) RETURNING pic_id" [userPicUrlParam]
          day <- getOrdinalDay  
          [Only userId] <- query conn "INSERT INTO users ( password, first_name , last_name , user_pic_id , user_create_date, admin) VALUES ( ?,?,?,?,?, true ) RETURNING user_id" [ passwordParam , firstNameParam, lastNameParam, pack (show picId), pack day ]
          okHelper $ lazyByteString $ encode (CreateUserResponse {user_id = userId , first_name = firstNameParam , last_name = lastNameParam, user_pic_id = picId, user_pic_url = pack ("http://localhost:3000/pic_" ++ show picId)})
        False -> send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["createAuthor"]        -> do
      let adminIdParam    = fromJust . fromJust . lookup "admin_id"    $ queryToQueryText $ queryString req
      let passwordParam   = fromJust . fromJust . lookup "password"    $ queryToQueryText $ queryString req
      let userIdParam     = fromJust . fromJust . lookup "user_id"     $ queryToQueryText $ queryString req
      let authorInfoParam = fromJust . fromJust . lookup "author_info" $ queryToQueryText $ queryString req
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only admPassword] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case admBool of
        True -> do
          case (unpack $ passwordParam) == admPassword of
            True -> do
              [Only authorId] <- query conn "INSERT INTO authors ( user_id , author_info) VALUES ( ?,?) RETURNING author_id" [ userIdParam, authorInfoParam]
              okHelper $ lazyByteString $ encode (CreateAuthorResponse { author_id = authorId , auth_user_id = read $ unpack $ userIdParam , author_info = authorInfoParam})
            False -> do
              okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = False})
        False ->  send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["createCategory"]        -> do
      let adminIdParam    = fromJust . fromJust . lookup "admin_id"      $ queryToQueryText $ queryString req
      let passwordParam   = fromJust . fromJust . lookup "password"      $ queryToQueryText $ queryString req
      let catNameParam    = fromJust . fromJust . lookup "category_name" $ queryToQueryText $ queryString req
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only admPassword] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case admBool of
        True -> do
          case (unpack $ passwordParam) == admPassword of
            True -> do
              [Only catId] <- query conn "INSERT INTO categories ( category_name) VALUES (?) RETURNING category_id " [ catNameParam]
              [Only superCatId] <- query conn "SELECT COALESCE (super_category_id, '0') AS super_category_id FROM categories WHERE category_id = ?" [pack . show $ catId]
              okHelper $ lazyByteString $ encode (CreateCatResponse { cat_id = catId , cat_name =  catNameParam , super_cat = pack . prettyNull . show $ (superCatId :: Int)})
            False -> do
              okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = False})
        False ->  send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["createSubCategory"]        -> do
      let adminIdParam    = fromJust . fromJust . lookup "admin_id"          $ queryToQueryText $ queryString req
      let passwordParam   = fromJust . fromJust . lookup "password"          $ queryToQueryText $ queryString req
      let catNameParam    = fromJust . fromJust . lookup "category_name"     $ queryToQueryText $ queryString req
      let superCatIdParam = fromJust . fromJust . lookup "super_category_id" $ queryToQueryText $ queryString req
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only admPassword] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case admBool of
        True -> do
          case (unpack $ passwordParam) == admPassword of
            True -> do
              [Only catId] <- query conn "INSERT INTO categories ( category_name, super_category_id) VALUES (?,?) RETURNING category_id" [ catNameParam, superCatIdParam ]
              xs <- foo catId
              okHelper $ lazyByteString $ encode $ moo xs
            False -> do
              okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = False})
        False ->  send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["getCategory", catId]      -> do
      xs <- foo (read $ unpack $ catId)
      okHelper $ lazyByteString $ encode $ moo xs
    ["createTag"]  -> do
      let adminIdParam    = fromJust . fromJust . lookup "admin_id"          $ queryToQueryText $ queryString req
      let passwordParam   = fromJust . fromJust . lookup "password"          $ queryToQueryText $ queryString req
      let tagNameParam    = fromJust . fromJust . lookup "tag_name"     $ queryToQueryText $ queryString req
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only admPassword] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case admBool of
        True -> do
          case (unpack $ passwordParam) == admPassword of
            True -> do
              [Only tagId] <- query conn "INSERT INTO tags ( tag_name) VALUES (?) RETURNING tag_id" [ tagNameParam ]
              okHelper $ lazyByteString $ encode $ CreateTagResponse tagId tagNameParam
            False -> do
              okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = False})
        False ->  send . responseBuilder status404 [] $ "Status 404 Not Found"
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
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [usIdParam]
      case pwdParam == pwd of
        True -> do
          [Only check]  <- query conn "SELECT EXISTS (SELECT author_id FROM authors WHERE user_id = ?) AS author_id" [usIdParam]
          case check of
            True -> do
              [Only authorId] <- (query conn "SELECT author_id FROM authors WHERE user_id = ?" [usIdParam]) :: IO [Only Integer]
              [Only picId]  <- query conn "INSERT INTO pics ( pic_url ) VALUES (?) RETURNING pic_id" [draftMainPicUrlParam]
              [Only draftId] <- query conn "INSERT INTO drafts (author_id, draft_name, draft_category_id, draft_text, draft_main_pic_id) VALUES (?,?,?,?,?) RETURNING draft_id" [pack . show $ authorId,draftNameParam,pack . show $ draftCatIdParam,draftTextParam,pack . show $ picId]
              mapM (koo draftId) draftTagsIds
              xs <- foo draftCatIdParam
              draftPicsIds <- mapM goo draftPicsUrls
              mapM (poo draftId) draftPicsIds
              ys <- mapM roo draftTagsIds
              okHelper $ lazyByteString $ encode $ CreateNewDraftResponse { draft_id = draftId, post_id2 = "NULL" , author_id2 = authorId, draft_name2 = draftNameParam , draft_cat =  moo xs , draft_text2 = draftTextParam , draft_main_pic_id =  picId , draft_main_pic_url2 = draftMainPicUrlParam , draft_tags = ys, draft_pics2 =  loo draftPicsIds draftPicsUrls}
            False -> okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = False})
        False -> okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = False})
    ["createPostsDraft"]  -> do
      let usIdParam   = fromJust . fromJust . lookup "user_id"          $ queryToQueryText $ queryString req
      let pwdParam = fromJust . fromJust . lookup "password"          $ queryToQueryText $ queryString req
      let postIdParam  = fromJust . fromJust . lookup "post_id"     $ queryToQueryText $ queryString req
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [usIdParam]
      case pwdParam == pwd of
        True -> do
          [Only check]  <- query conn "SELECT EXISTS (SELECT author_id FROM authors WHERE user_id = ?) AS author_id" [usIdParam]
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
                  xs <- query conn "SELECT pic_id FROM postspics WHERE post_id = ?" [postIdParam]
                  let picsIds = fmap fromOnly xs
                  ys <- query conn "SELECT tag_id FROM poststags WHERE post_id = ?" [postIdParam] 
                  let tagsIds = fmap fromOnly ys
                  mapM (koo draftId) tagsIds
                  mapM (poo draftId) picsIds
                  zs <- foo postCatId
                  hs <- mapM roo tagsIds
                  okHelper $ lazyByteString $ encode $ CreatePostsDraftResponse { draft_id5 = draftId, post_id5 = read . unpack $ postIdParam, author_id5 = authorUsId, draft_name5 = postName , draft_cat5 =  moo zs , draft_text5 = postText , draft_main_pic_id5 =  postMainPicId , draft_main_pic_url5 = voo postMainPicId , draft_tags5 = hs, draft_pics5 =  loo picsIds (fmap voo picsIds)}
                False -> okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = False})
            False -> okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = False})
        False -> okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = False})    
    ["publishDraft"]  -> do
      let usIdParam   = fromJust . fromJust . lookup "user_id"          $ queryToQueryText $ queryString req
      let passwordParam = fromJust . fromJust . lookup "password"          $ queryToQueryText $ queryString req
      let draftIdParam  = fromJust . fromJust . lookup "draft_id"     $ queryToQueryText $ queryString req
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only check]  <- query conn "SELECT EXISTS (SELECT author_id FROM authors WHERE user_id = ?) AS author_id" [usIdParam]
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
            day <- getGregorianDay
            [Only postId] <- query conn "INSERT INTO posts (author_id, post_name, post_create_date, post_category_id, post_text, post_main_pic_id) VALUES (?,?,?,?,?,?) RETURNING post_id" [pack . show $ authorUsId,draftName,pack day,pack . show $ draftCatId,draftText,pack . show $ draftMainPicId]
            xs <- query conn "SELECT pic_id FROM draftspics WHERE draft_id = ?" [draftIdParam]
            let picsIds = fmap fromOnly xs
            mapM (qoo postId) picsIds
            ys <- query conn "SELECT tag_id FROM draftstags WHERE draft_id = ?" [draftIdParam] 
            let tagsIds = fmap fromOnly ys
            mapM (woo postId) tagsIds
            zs <- foo draftCatId
            hs <- mapM roo tagsIds
            okHelper $ lazyByteString $ encode $ PublishDraftResponse { post_id = postId, author_id4 = authorUsId, post_name = draftName , post_create_date = (pack day), post_cat = moo zs, post_text = draftText, post_main_pic_id = draftMainPicId, post_main_pic_url = voo draftMainPicId, post_pics = loo picsIds (fmap voo picsIds), post_tags = hs}
          _ -> do 
            [Only postId] <- query conn "SELECT post_id FROM drafts WHERE draft_id = ?" [draftIdParam]
            [Only draftName] <- query conn "SELECT draft_name FROM drafts WHERE draft_id = ?" [draftIdParam]
            [Only draftCatId] <- query conn "SELECT draft_category_id FROM drafts WHERE draft_id = ?" [draftIdParam]
            [Only draftText] <- query conn "SELECT draft_text FROM drafts WHERE draft_id = ?" [draftIdParam]
            [Only draftMainPicId] <- query conn "SELECT draft_main_pic_id FROM drafts WHERE draft_id = ?" [draftIdParam]
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
            okHelper $ lazyByteString $ encode $ PublishDraftResponse { post_id = postId, author_id4 = authorUsId, post_name = draftName , post_create_date = pack . showGregorian $ postCreateDate, post_cat = moo zs, post_text = draftText, post_main_pic_id = draftMainPicId, post_main_pic_url = voo draftMainPicId, post_pics = loo picsIds (fmap voo picsIds), post_tags = hs}
        False -> okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = False})
    ["createComment"]  -> do
      let usIdParam    = fromJust . fromJust . lookup "user_id"          $ queryToQueryText $ queryString req
      let passwordParam   = fromJust . fromJust . lookup "password"          $ queryToQueryText $ queryString req
      let postIdParam    = fromJust . fromJust . lookup "post_id"     $ queryToQueryText $ queryString req
      let commentTextParam    = fromJust . fromJust . lookup "comment_text"     $ queryToQueryText $ queryString req
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only usPassword] <- query conn "SELECT password FROM users WHERE user_id = ? " [usIdParam]
      case passwordParam == usPassword of
        True -> do
          [Only commentId] <- query conn "INSERT INTO comments (comment_text, post_id, user_id) VALUES (?,?,?) RETURNING comment_id" [ commentTextParam, postIdParam, usIdParam ]
          okHelper $ lazyByteString $ encode $ CreateCommentResponse { comment_id = commentId , comment_text = commentTextParam, post_id6 = read . unpack $ postIdParam, user_id6 = read . unpack $ usIdParam}
        False ->  send . responseBuilder status404 [] $ "Status 404 Not Found"



{-    ["getDraft"]  -> do
      let adminIdParam    = fromJust . fromJust . lookup "admin_id"          $ queryToQueryText $ queryString req
      let passwordParam   = fromJust . fromJust . lookup "password"          $ queryToQueryText $ queryString req
      let draftIdParam    = fromJust . fromJust . lookup "draft_id"     $ queryToQueryText $ queryString req
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only admPassword] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case admBool of
        True -> do
          case (unpack $ passwordParam) == admPassword of
            True -> do
              [Only catId] <- query conn "SELECT admin FROM users WHERE user_id = ? " [draftIdParam]
              xs <- foo catId
              okHelper $ lazyByteString $ encode $ moo xs
            False -> do
              okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = False})
        False ->  send . responseBuilder status404 [] $ "Status 404 Not Found"
-}
    
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
  return $ CreateTagResponse tagId tagName

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

moo [(x,y)] = CreateCatResponse { cat_id = x , cat_name =  y , super_cat = "NULL"}
moo ((x,y):xs) = CreateSubCatResponse { subCat_id = x , subCat_name =  y , super_category = moo xs}

prettyNull :: String -> String
prettyNull "0" = "NULL" 
prettyNull a   = a       
      
      
main :: IO ()
main = do
  createDbStructure
  run 3000 application



