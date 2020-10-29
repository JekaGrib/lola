{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
import           Data.String                    ( fromString )

defaultPictureUrl :: Text
defaultPictureUrl = "https://cdn.pixabay.com/photo/2020/01/14/09/20/anonym-4764566_960_720.jpg"
defUsId = 1
defPicId = 1
defAuthId = 1

commentNumberLimit = 20

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
  execute_ conn "CREATE TABLE authors ( author_id BIGSERIAL PRIMARY KEY NOT NULL, author_info VARCHAR(1000) NOT NULL, user_id BIGINT NOT NULL REFERENCES users(user_id))"
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
  return [picId,userId,authorId]
  



data CreateUserResponse = CreateUserResponse {
      user_id      :: Integer
    , first_name   :: Text
    , last_name    :: Text
    , user_pic_id  :: Integer
    , user_pic_url :: Text
    , user_create_date :: Text
    } deriving Show

instance ToJSON CreateUserResponse where
    toJSON (CreateUserResponse user_id first_name last_name user_pic_id user_pic_url user_create_date) =
        object ["user_id" .= user_id, "first_name" .= first_name, "last_name" .= last_name, "user_pic_id" .= user_pic_id, "user_pic_url" .= user_pic_url, "user_create_date" .= user_create_date]
    toEncoding (CreateUserResponse user_id first_name last_name user_pic_id user_pic_url user_create_date) =
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
    , author2   :: CreateAuthorResponse
    , draft_name2    :: Text
    , draft_cat :: CreateCatResponse
    , draft_text2  :: Text
    , draft_main_pic_id  :: Integer
    , draft_main_pic_url2 :: Text
    , draft_pics2 :: [PicIdUrl]
    , draft_tags ::  [CreateTagResponse]
    } deriving Show

instance ToJSON CreateNewDraftResponse where
    toJSON (CreateNewDraftResponse draft_id post_id2 author2 draft_name2 draft_cat draft_text2 draft_main_pic_id draft_main_pic_url2 draft_pics2 draft_tags) =
        object ["draft_id" .= draft_id, "post_id" .= post_id2, "author" .= author2, "draft_name" .= draft_name2, "draft_category" .= draft_cat, "draft_text" .= draft_text2, "draft_main_pic_id" .= draft_main_pic_id, "draft_main_pic_url" .= draft_main_pic_url2, "draft_pics" .= draft_pics2, "draft_tags" .= draft_tags]
    toEncoding (CreateNewDraftResponse draft_id post_id2 author2 draft_name2 draft_cat draft_text2 draft_main_pic_id draft_main_pic_url2 draft_pics2 draft_tags ) =
        pairs ("draft_id" .= draft_id <> "post_id" .= post_id2 <> "author" .= author2 <> "draft_name" .= draft_name2 <> "draft_category" .= draft_cat <> "draft_text" .= draft_text2 <> "draft_main_pic_id" .= draft_main_pic_id <> "draft_main_pic_url" .= draft_main_pic_url2 <> "draft_pics" .= draft_pics2 <> "draft_tags" .= draft_tags)


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
    , author5   :: CreateAuthorResponse
    , draft_name5    :: Text
    , draft_cat5 :: CreateCatResponse
    , draft_text5  :: Text
    , draft_main_pic_id5  :: Integer
    , draft_main_pic_url5 :: Text
    , draft_pics5 :: [PicIdUrl]
    , draft_tags5 ::  [CreateTagResponse]
    } deriving Show

instance ToJSON CreatePostsDraftResponse where
    toJSON (CreatePostsDraftResponse draft_id5 post_id5 author5 draft_name5 draft_cat5 draft_text5 draft_main_pic_id5 draft_main_pic_url5 draft_pics5 draft_tags5) =
        object ["draft_id" .= draft_id5, "post_id" .= post_id5, "author" .= author5, "draft_name" .= draft_name5, "draft_category" .= draft_cat5, "draft_text" .= draft_text5, "draft_main_pic_id" .= draft_main_pic_id5, "draft_main_pic_url" .= draft_main_pic_url5, "draft_pics" .= draft_pics5, "draft_tags" .= draft_tags5]
    toEncoding (CreatePostsDraftResponse draft_id5 post_id5 author5 draft_name5 draft_cat5 draft_text5 draft_main_pic_id5 draft_main_pic_url5 draft_pics5 draft_tags5 ) =
        pairs ("draft_id" .= draft_id5 <> "post_id" .= post_id5 <> "author" .= author5 <> "draft_name" .= draft_name5 <> "draft_category" .= draft_cat5 <> "draft_text" .= draft_text5 <> "draft_main_pic_id" .= draft_main_pic_id5 <> "draft_main_pic_url" .= draft_main_pic_url5 <> "draft_pics" .= draft_pics5 <> "draft_tags" .= draft_tags5)

data PublishDraftResponse = PublishDraftResponse {
      post_id      :: Integer
    , author4   :: CreateAuthorResponse
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
    toJSON (PublishDraftResponse post_id author4 post_name post_create_date post_cat post_text post_main_pic_id post_main_pic_url post_pics post_tags) =
        object ["post_id" .= post_id, "author" .= author4, "post_name" .= post_name, "post_create_date" .= post_create_date, "post_category" .= post_cat, "post_text" .= post_text, "post_main_pic_id" .= post_main_pic_id, "post_main_pic_url" .= post_main_pic_url, "post_pics" .= post_pics, "post_tags" .= post_tags]
    toEncoding (PublishDraftResponse post_id author4 post_name post_create_date post_cat post_text post_main_pic_id post_main_pic_url post_pics post_tags) =
        pairs ("post_id" .= post_id <> "author" .= author4  <> "post_name" .= post_name <> "post_create_date" .= post_create_date <> "post_category" .= post_cat <> "post_text" .= post_text <> "post_main_pic_id" .= post_main_pic_id <> "post_main_pic_url" .= post_main_pic_url <> "post_pics" .= post_pics <> "post_tags" .= post_tags)



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

data CommentsResponse = CommentsResponse {
      page   :: Integer
    , comments :: [CreateCommentResponse]
    } deriving Show

instance ToJSON CommentsResponse where
    toJSON (CommentsResponse page comments) =
        object ["page" .= page, "comments" .= comments]
    toEncoding (CommentsResponse page comments) =
        pairs ( "page" .= page <> "comments" .= comments )

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
      okHelper $ lazyByteString $ encode (CreateUserResponse {user_id = userId , first_name = firstNameParam , last_name = lastNameParam, user_pic_id = picId, user_pic_url = voo picId, user_create_date = pack day })
    ["getUser", userId]      -> do
      [Only firstName] <- query conn "SELECT first_name FROM users WHERE user_id = ? " [userId ]
      [Only lastName] <- query conn "SELECT last_name FROM users WHERE user_id = ? " [userId]
      [Only picId] <- query conn "SELECT user_pic_id FROM users WHERE user_id = ? " [userId]
      [Only userCreateDate] <- (query conn "SELECT user_create_date FROM users WHERE user_id = ?" [userId]) :: IO [Only Day]
      okHelper $ lazyByteString $ encode (CreateUserResponse {user_id = read . unpack $ userId , first_name = firstName , last_name = lastName, user_pic_id = picId, user_pic_url = voo picId, user_create_date = pack . showGregorian $ userCreateDate})
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
          okHelper $ lazyByteString $ encode (CreateUserResponse {user_id = userId , first_name = firstNameParam , last_name = lastNameParam, user_pic_id = picId, user_pic_url = voo picId,user_create_date = pack day })
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
          okHelper $ lazyByteString $ encode (CreateAuthorResponse { author_id = authorId , auth_user_id = read $ unpack $ userIdParam , author_info = authorInfoParam})
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
          okHelper $ lazyByteString $ encode (CreateAuthorResponse { author_id = read . unpack $ authorIdParam , auth_user_id = userId , author_info = authorInfo})
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
          okHelper $ lazyByteString $ encode (CreateAuthorResponse { author_id = read . unpack $ authorIdParam , auth_user_id = userId , author_info = authorInfoParam})
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
          okHelper $ lazyByteString $ encode (CreateCatResponse { cat_id = catId , cat_name =  catNameParam , super_cat = pack . prettyNull . show $ (superCatId :: Int)})
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
          okHelper $ lazyByteString $ encode $ moo xs
        "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["getCategory", catId] -> do
      xs <- foo (read $ unpack $ catId)
      okHelper $ lazyByteString $ encode $ moo xs
    ["createTag"]  -> do
      let adminIdParam    = fromJust . fromJust . lookup "admin_id"          $ queryToQueryText $ queryString req
      let pwdParam   = fromJust . fromJust . lookup "password"          $ queryToQueryText $ queryString req
      let tagNameParam    = fromJust . fromJust . lookup "tag_name"     $ queryToQueryText $ queryString req
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminIdParam]
      [Only pwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminIdParam]
      case zoo pwdParam pwd admBool of
        "Success" -> do
          [Only tagId] <- query conn "INSERT INTO tags ( tag_name) VALUES (?) RETURNING tag_id" [ tagNameParam ]
          okHelper $ lazyByteString $ encode $ CreateTagResponse tagId tagNameParam
        "INVALID pwd, admin = True "     -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "valid pwd, user is NOT admin"   -> send . responseBuilder status404 [] $ "Status 404 Not Found"
        "INVALID pwd, user is NOT admin" -> send . responseBuilder status404 [] $ "Status 404 Not Found"
    ["getTag",tagId]  -> do
      [Only tagName] <- query conn "SELECT tag_name FROM tags WHERE tag_id = ? " [ tagId ]
      okHelper $ lazyByteString $ encode $ CreateTagResponse (read . unpack $ tagId) tagName
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
          okHelper $ lazyByteString $ encode (CreateTagResponse { tag_id = read . unpack $ tagIdParam , tag_name = tagNameParam})
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
              okHelper $ lazyByteString $ encode $ CreateNewDraftResponse { draft_id = draftId, post_id2 = "NULL" , author2 = CreateAuthorResponse authorId usIdParam authorInfo, draft_name2 = draftNameParam , draft_cat =  moo xs , draft_text2 = draftTextParam , draft_main_pic_id =  picId , draft_main_pic_url2 = draftMainPicUrlParam , draft_tags = ys, draft_pics2 =  loo draftPicsIds draftPicsUrls}
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
                  okHelper $ lazyByteString $ encode $ CreatePostsDraftResponse { draft_id5 = draftId, post_id5 = read . unpack $ postIdParam, author5 = CreateAuthorResponse authorUsId (read . unpack $ usIdParam) authorInfo  , draft_name5 = postName , draft_cat5 =  moo zs , draft_text5 = postText , draft_main_pic_id5 =  postMainPicId , draft_main_pic_url5 = voo postMainPicId , draft_tags5 = hs, draft_pics5 =  loo picsIds (fmap voo picsIds)}
                False -> okHelper $ lazyByteString $ encode (OkResponse {ok = False})
            False -> okHelper $ lazyByteString $ encode (OkResponse {ok = False})
        False -> okHelper $ lazyByteString $ encode (OkResponse {ok = False})    
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
            okHelper $ lazyByteString $ encode $ PublishDraftResponse { post_id = postId, author4 = CreateAuthorResponse authorUsId (read . unpack $ usIdParam) authorInfo, post_name = draftName , post_create_date = (pack day), post_cat = moo zs, post_text = draftText, post_main_pic_id = draftMainPicId, post_main_pic_url = voo draftMainPicId, post_pics = loo picsIds (fmap voo picsIds), post_tags = hs}
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
            okHelper $ lazyByteString $ encode $ PublishDraftResponse { post_id = postId, author4 = CreateAuthorResponse authorUsId (read . unpack $ usIdParam) authorInfo, post_name = draftName , post_create_date = pack . showGregorian $ postCreateDate, post_cat = moo zs, post_text = draftText, post_main_pic_id = draftMainPicId, post_main_pic_url = voo draftMainPicId, post_pics = loo picsIds (fmap voo picsIds), post_tags = hs}
        False -> okHelper $ lazyByteString $ encode (OkResponse {ok = False})
    ["createComment"]  -> do
      case fmap (isExistParam req) ["user_id","password","post_id","comment_text"] of
        [True,True,True,True] -> do
          case fmap (parseParam req) ["user_id","password","post_id","comment_text"] of
            [Just usIdParam,Just pwdParam,Just postIdParam,Just commentTextParam] -> do
              [Only usPwd] <- query conn "SELECT password FROM users WHERE user_id = ? " [usIdParam]
              case pwdParam == usPwd of
                True -> do
                  [Only commentId] <- query conn "INSERT INTO comments (comment_text, post_id, user_id) VALUES (?,?,?) RETURNING comment_id" [ commentTextParam, postIdParam, usIdParam ]
                  okHelper $ lazyByteString $ encode $ CreateCommentResponse { comment_id = commentId , comment_text = commentTextParam, post_id6 = read . unpack $ postIdParam, user_id6 = read . unpack $ usIdParam}
                False ->  okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "INVALID password"}
            _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t parse query parameter"}
        _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t find query parameter"}
    ["getComments"] -> do
      case fmap (isExistParam req) ["post_id","page"] of
        [True,True] -> do
          case fmap (parseParam req) ["post_id","page"] of
            [Just postIdParam, Just pageParam] -> do
              let pageNum = read . unpack $ pageParam
              commentsIds <- selectLimitListFromDb conn "comments" ("post_id",postIdParam) "comment_id" pageNum commentNumberLimit
              commentsTexts <- mapM (reverseSelectFromDb conn "comments" "comment_text" "comment_id") (fmap (pack . show) commentsIds) 
              postsIds <- mapM (reverseSelectFromDb conn "comments" "post_id" "comment_id") (fmap (pack . show) commentsIds)
              usIds <- mapM (reverseSelectFromDb conn "comments" "user_id" "comment_id") (fmap (pack . show) commentsIds)
              okHelper $ lazyByteString $ encode $ CommentsResponse {page = pageNum, comments = coo commentsIds commentsTexts postsIds usIds }
    ["updateMyComment"]  -> do
      case fmap (isExistParam req) ["user_id","password","comment_id","comment_text"] of
        [True,True,True,True] -> do
          case fmap (parseParam req) ["user_id","password","comment_id","comment_text"] of
            [Just usIdParam,Just pwdParam,Just commentIdParam,Just commentTextParam] -> do
              pwd <- selectFromDb conn "users" ("user_id",usIdParam) "password"
              case pwd == pwdParam of
                True -> do
                  usCommentId <- (selectFromDb conn "comments" ("comment_id",commentIdParam) "user_id") :: IO Integer
                  case usCommentId == (read . unpack $ usIdParam) of
                    True -> do
                      execute conn "DELETE FROM comments WHERE comment_id = ?" [commentIdParam]
                      postId  <- (selectFromDb conn "comments" ("comment_id",commentIdParam) "post_id") :: IO Integer
                      okHelper $ lazyByteString $ encode $ CreateCommentResponse { comment_id = (read . unpack $ commentIdParam) , comment_text = commentTextParam, post_id6 = postId, user_id6 = usCommentId}
                    _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "User cannot update this comment"}
                _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "INVALID password"}
            _ -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Can`t parse query parameter"}
    ["deleteComment"]  -> do
      case fmap (isExistParam req) ["user_id","admin_id","password","comment_id"] of
        [True,True,True,True] -> okHelper $ lazyByteString $ encode $ OkInfoResponse {ok7 = False, info7 = pack $ "Too much query parameters"}
        [True,False,True,True] -> do
          case fmap (parseParam req) ["user_id","password","comment_id"] of
            [Just usIdParam,Just pwdParam,Just commentIdParam] -> do
              pwd <- selectFromDb conn "users" ("user_id",usIdParam) "password"
              case pwd == pwdParam of
                True -> do
                  usCommentId <- (selectFromDb conn "comments" ("comment_id",commentIdParam) "user_id") :: IO Integer
                  postId      <- (selectFromDb conn "comments" ("comment_id",commentIdParam) "post_id") :: IO Integer
                  authPostId  <- (selectFromDb conn "posts" ("post_id",pack . show $ postId) "author_id") :: IO Integer
                  usPostId  <- (selectFromDb conn "authors" ("author_id",pack . show $ authPostId) "user_id") :: IO Integer
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
              okHelper $ lazyByteString $ encode (OkResponse {ok = False})
        False ->  send . responseBuilder status404 [] $ "Status 404 Not Found"
-}


isExistParam req txt = case lookup txt $ queryToQueryText $ queryString req of
  Just _  -> True
  Nothing -> False

parseParam req txt = fromJust . lookup txt $ queryToQueryText $ queryString req 

--selectFromDb :: (Database.PostgreSQL.Simple.FromField.FromField a) => Connection -> String -> (String,Text) -> String -> IO a
selectFromDb conn table (eqParamName,eqParamValue) param = do
  [Only value] <- query conn (fromString $ "SELECT " ++ param ++ " FROM " ++ table ++ " WHERE " ++ eqParamName ++ " = ?") [eqParamValue]
  return value

reverseSelectFromDb conn table param eqParamName eqParamValue = selectFromDb conn table (eqParamName,eqParamValue) param

--selectListFromDb :: (Database.PostgreSQL.Simple.FromField.FromField a) => Connection -> String -> (String,Text) -> String -> IO [a]
selectListFromDb conn table (eqParamName,eqParamValue) param  = do
  xs <- query conn (fromString $ "SELECT " ++ param ++ " FROM " ++ table ++ " WHERE " ++ eqParamName ++ " = ?") [eqParamValue]
  return (fmap fromOnly xs)


--selectLimitListFromDb :: (Database.PostgreSQL.Simple.FromField.FromField a) =>  Connection -> String -> (String,Text) -> String -> Integer -> Integer -> IO [a]
selectLimitListFromDb conn table (eqParamName,eqParamValue) param page limitNumber = do
  xs <- query conn (fromString $ "SELECT " ++ param ++ " FROM " ++ table ++ " WHERE " ++ eqParamName ++ " = ? OFFSET " ++ show ((page-1)*limitNumber) ++ " LIMIT " ++ show (page*limitNumber)) [eqParamValue]
  return (fmap fromOnly xs)  



coo [a] [b] [c] [d] = [CreateCommentResponse a b c d]
coo (a:as) (b:bs) (c:cs) (d:ds) = (CreateCommentResponse a b c d) : coo as bs cs ds

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
  addDefaultParameters
  run 3000 application



