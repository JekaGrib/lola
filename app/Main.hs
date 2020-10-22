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
  execute_ conn "CREATE TABLE authors ( author_id BIGSERIAL PRIMARY KEY NOT NULL, author_info VARCHAR(1000))"
  print "kk"

createPicsTable :: IO ()
createPicsTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE pics ( pic_id BIGSERIAL PRIMARY KEY NOT NULL, pic_url VARCHAR(1000) NOT NULL)"
  print "kk"

createUsersTable :: IO ()
createUsersTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE users ( user_id BIGSERIAL PRIMARY KEY NOT NULL, password VARCHAR(50) NOT NULL, first_name VARCHAR(50) NOT NULL, last_name  VARCHAR(50) NOT NULL, user_pic_id BIGINT NOT NULL REFERENCES pics(pic_id), user_create_date DATE NOT NULL, admin boolean NOT NULL, author_id BIGINT REFERENCES authors(author_id), UNIQUE (author_id))"
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

createDbStructure = do
  createAuthorsTable
  createPicsTable
  createUsersTable
  createTagsTable
  createCategoriesTable
  createPostsTable
  createCommentsTable
  createPostsPicsTable
  createPostsTagsTable
  createDraftsTable
  createDraftsPicsTable
  createDraftsTagsTable

data CreateUserResponse = CreateUserResponse {
      user_id      :: Integer
    , first_name   :: Text
    , last_name    :: Text
    , user_pic_id  :: Integer
    , user_pic_url :: Text
    } deriving Show

data DeleteUserResponse = DeleteUserResponse {ok :: Bool}

instance ToJSON CreateUserResponse where
    toJSON (CreateUserResponse user_id first_name last_name user_pic_id user_pic_url) =
        object ["user_id" .= user_id, "first_name" .= first_name, "last_name" .= last_name, "user_pic_id" .= user_pic_id, "user_pic_url" .= user_pic_url]
    toEncoding (CreateUserResponse user_id first_name last_name user_pic_id user_pic_url) =
        pairs ("user_id" .= user_id <> "first_name" .= first_name <> "last_name" .= last_name <> "user_pic_id" .= user_pic_id <> "user_pic_url" .= user_pic_url)

instance ToJSON DeleteUserResponse where
    toJSON (DeleteUserResponse ok) =
        object ["ok" .= ok]
    toEncoding (DeleteUserResponse ok) =
        pairs ("ok" .= ok )

application req send = do
  let okHelper = send . responseBuilder status200 [("Content-Type", "application/json; charset=utf-8")]
  case pathInfo req of
    ["createUser"]        -> do
      let passwordParam   = fromJust . fromJust . lookup     "password" $ queryToQueryText $ queryString req
      let firstNameParam  = fromJust . fromJust . lookup   "first_name" $ queryToQueryText $ queryString req
      let lastNameParam   = fromJust . fromJust . lookup    "last_name" $ queryToQueryText $ queryString req
      let userPicUrlParam = fromJust . fromJust . lookup "user_pic_url" $ queryToQueryText $ queryString req
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only picId]  <- query conn "INSERT INTO pics ( pic_url ) VALUES ( ? ) RETURNING pic_id" [userPicUrlParam]
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
      let userId        = fromJust . fromJust . lookup "user_id"  $ queryToQueryText $ queryString req
      let passwordParam = fromJust . fromJust . lookup "password" $ queryToQueryText $ queryString req
      let adminId       = fromJust . fromJust . lookup "admin_id" $ queryToQueryText $ queryString req
      conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
      [Only admBool] <- query conn "SELECT admin FROM users WHERE user_id = ? " [adminId]
      [Only admPassword] <- query conn "SELECT password FROM users WHERE user_id = ? " [adminId]
      case admBool of
        True -> do
          case (unpack $ adminId) == admPassword of
            True -> do
              execute conn "DELETE FROM users WHERE user_id = ? " [userId]
              okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = True})
            False -> do
              okHelper $ lazyByteString $ encode (DeleteUserResponse {ok = False})
        False ->  send . responseBuilder status404 [] $ "Status 404 Not Found"
              
        
      
      
main :: IO ()
main = do
  createDbStructure
  run 3000 application



