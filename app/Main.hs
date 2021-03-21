{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}




module Main where

import           Test.Hspec
import           Control.Monad.State            


import           App
import           Api
import           Logger
import           Network.Wai
import           Network.HTTP.Types             ( status200, status404, status301, movedPermanently301, http11 )
import           Network.HTTP.Types.URI         ( queryToQueryText )
import           Network.Wai.Handler.Warp       ( run )
import           Data.Text                      ( pack, unpack, Text, concat, toUpper, stripPrefix, isPrefixOf )
import           Data.ByteString.Builder        ( lazyByteString )
import           Database.PostgreSQL.Simple
import qualified Network.HTTP.Simple            as HT
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Calendar             ( showGregorian, Day )
import           Database.PostgreSQL.Simple.Time
import           Data.String                    ( fromString )
import           Control.Monad.Catch            ( catch, throwM, MonadCatch )
import qualified Data.Configurator              as C
import qualified Data.Configurator.Types        as C
import qualified Control.Exception              as E


pullConfig :: IO C.Config
pullConfig = do
  C.load [C.Required "./postApp.config"] 
    `E.catch` (\e -> putStrLn (show (e :: C.ConfigError)) >> return C.empty)
    `E.catch` (\e -> putStrLn (show (e :: C.KeyError   )) >> return C.empty)
    `E.catch` (\e -> putStrLn (show (e :: E.IOException  )) >> return C.empty)
    

defaultPictureUrl :: Text
defaultPictureUrl = "https://cdn.pixabay.com/photo/2020/01/14/09/20/anonym-4764566_960_720.jpg"
defUsId = 1
defPicId = 1
defAuthId = 1
defCatId = 1

commentNumberLimit = 20
draftNumberLimit = 5
postNumberLimit = 5


getTime :: IO String
getTime = do
  time    <- getZonedTime
  return $ show time     
   

createDefaultPicture :: IO Integer
createDefaultPicture = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  [Only picId] <- query conn "INSERT INTO pics ( pic_url ) VALUES (?) RETURNING pic_id " [defaultPictureUrl]
  return picId

createDefaultUser picId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  day <- App.getDay'  
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



addCreateAdminKey = do
  conn1 <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn1 "INSERT INTO key (create_admin_key) VALUES ( 'lola' ) "


addDefaultParameters = do
  addCreateAdminKey
  picId <- createDefaultPicture
  userId <- createDefaultUser picId
  authorId <- createDefaultAuthor userId
  createDefaultCategory
  return [picId,userId,authorId]
  

main :: IO ()
main = do
  addDefaultParameters
  time <- getTime                          
  let currLogPath = "./PostApp.LogSession: " ++ show time ++ " .log"
  conf           <- pullConfig
  defPicId       <- parseConfDefPicId   conf 
  defUsId       <- parseConfDefUsId   conf 
  defAuthId       <- parseConfDefAuthId   conf 
  defCatId       <- parseConfDefCatId   conf 
  let handleLog = LogHandle (LogConfig DEBUG) (logger handleLog currLogPath)
  let config = Config defPicId defUsId defAuthId defCatId 
  run 3000 (application config handleLog)

parseConfDefPicId :: C.Config -> IO Integer
parseConfDefPicId conf = do
  str <- ((C.lookup conf "postApp.defaultPictureId") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> inputDefId
    Just x  -> return x

parseConfDefUsId :: C.Config -> IO Integer
parseConfDefUsId conf = do
  str <- ((C.lookup conf "postApp.defaultUserId") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> inputDefId
    Just x  -> return x

parseConfDefAuthId :: C.Config -> IO Integer
parseConfDefAuthId conf = do
  str <- ((C.lookup conf "postApp.defaultAuthorId") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> inputDefId
    Just x  -> return x

parseConfDefCatId :: C.Config -> IO Integer
parseConfDefCatId conf = do
  str <- ((C.lookup conf "postApp.defaultCategoryId") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> inputDefId
    Just x  -> return x

inputDefId :: IO Integer
inputDefId = do
  putStrLn "Can`t parse value \"startN\" from configuration file or command line\nPlease, enter start number of repeats. Number from 1 to 5"
  input <- getLine
  case input of
    "1" -> return 1
    "2" -> return 2
    "3" -> return 3
    "4" -> return 4
    "5" -> return 5
    _   -> inputDefId