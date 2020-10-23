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

getDay :: IO String
getDay = do
  time    <- getZonedTime
  let day = showOrdinalDate . localDay . zonedTimeToLocalTime $ time
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
  execute_ conn "CREATE TABLE posts ( post_id BIGSERIAL PRIMARY KEY NOT NULL, post_name VARCHAR(100) NOT NULL, post_create_date DATE NOT NULL, post_category_id BIGINT NOT NULL REFERENCES categories(category_id), post_text VARCHAR(10000) NOT NULL, post_main_pic_id BIGINT NOT NULL REFERENCES pics(pic_id))"
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
  execute_ conn "CREATE TABLE drafts ( draft_id BIGSERIAL PRIMARY KEY NOT NULL, post_id BIGINT REFERENCES posts(post_id), draft_name VARCHAR(100) NOT NULL, draft_category_id BIGINT NOT NULL REFERENCES categories(category_id), draft_text VARCHAR(10000) NOT NULL, draft_main_pic_id BIGINT NOT NULL REFERENCES pics(pic_id))"
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
      day <- getDay  
      [Only userId] <- query conn "INSERT INTO users ( password, first_name , last_name , user_pic_id , user_create_date, admin) VALUES ( ?,?,?,?,?, false ) RETURNING user_id" [ passwordParam , firstNameParam, lastNameParam, pack (show picId), pack day ]
      okHelper $ lazyByteString $ encode (CreateUserResponse {user_id = userId , first_name = firstNameParam , last_name = lastNameParam, user_pic_id = picId, user_pic_url = pack ("http://localhost:3000/pic_" ++ show picId)})
    ["getUser", _]      -> do
      let userId = (pathInfo req) !! 1
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
          day <- getDay  
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
    ["getCategory", _]      -> do
      let catId = (pathInfo req) !! 1
      xs <- foo (read $ unpack $ catId)
      okHelper $ lazyByteString $ encode $ moo xs
   
    




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



