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
import qualified Data.ByteString.Lazy           as BSL
import           Codec.Picture                  ( decodeImage )


pullConfig :: IO C.Config
pullConfig = do
  C.load [C.Required "./postApp.config"] 
    `E.catch` (\e -> putStrLn (show (e :: C.ConfigError)) >> return C.empty)
    `E.catch` (\e -> putStrLn (show (e :: C.KeyError   )) >> return C.empty)
    `E.catch` (\e -> putStrLn (show (e :: E.IOException  )) >> return C.empty)
    

{-defaultPictureUrl :: Text
defaultPictureUrl = "https://cdn.pixabay.com/photo/2020/01/14/09/20/anonym-4764566_960_720.jpg"
-}


getTime :: IO String
getTime = do
  time    <- getZonedTime
  return $ show time     
   
  

main :: IO ()
main = do
  time <- getTime                          
  let currLogPath = "./PostApp.LogSession: " ++ show time ++ " .log"
  conn <- connectPostgreSQL (fromString $ "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'") 
  conf            <- pullConfig
  defPicId        <- parseConfDefPicId    conf conn 
  defUsId         <- parseConfDefUsId     conf conn defPicId
  defAuthId       <- parseConfDefAuthId   conf conn defUsId
  defCatId        <- parseConfDefCatId    conf conn
  commNumLimit    <- parseConfCommLimit   conf
  draftsNumLimit  <- parseConfDraftsLimit conf
  postsNumLimit   <- parseConfPostsLimit  conf
  let handleLog = LogHandle (LogConfig DEBUG) (logger handleLog currLogPath)
  let config = Config defPicId defUsId defAuthId defCatId commNumLimit draftsNumLimit postsNumLimit
  run 3000 (application config handleLog)


parseConfDefPicId :: C.Config -> Connection -> IO Integer
parseConfDefPicId conf conn = do
  str <- ((C.lookup conf "defaultValues.defaultPictureId") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> do
      x <- inputIntegerOr "defaultPictureId" crateNewDefPic
      checkExistId conn "pics" "pic_id" "pic_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultPictureId" crateNewDefPic)
    Just x  -> do
      checkExistId conn "pics" "pic_id" "pic_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultPictureId" crateNewDefPic)

parseConfDefUsId :: C.Config -> Connection -> Integer -> IO Integer
parseConfDefUsId conf conn defPicId = do
  str <- ((C.lookup conf "defaultValues.defaultUserId") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> do
      x <- inputIntegerOr "defaultUserId" (crateNewDefUser defPicId)
      checkExistId conn "users" "user_id" "user_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultUserId" (crateNewDefUser defPicId))
    Just x  -> do
      checkExistId conn "users" "user_id" "user_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultUserId" (crateNewDefUser defPicId))

parseConfDefAuthId :: C.Config -> Connection -> Integer -> IO Integer
parseConfDefAuthId conf conn defUsId = do
  str <- ((C.lookup conf "defaultValues.defaultAuthorId") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> do
      x <- inputIntegerOr "defaultAuthorId" (crateNewDefAuthor defUsId)
      checkExistId conn "authors" "author_id" "author_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultAuthorId" (crateNewDefAuthor defUsId))
    Just x  -> do
      checkExistId conn "authors" "author_id" "author_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultAuthorId" (crateNewDefAuthor defUsId))

parseConfDefCatId :: C.Config -> Connection -> IO Integer
parseConfDefCatId conf conn = do
  str <- ((C.lookup conf "defaultValues.defaultCategoryId") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> do
      x <- inputIntegerOr "defaultCategoryId" crateNewDefCat
      checkExistId conn "categories" "category_id" "category_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultCategoryId" crateNewDefCat)
    Just x  -> do
      checkExistId conn "categories" "category_id" "category_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultCategoryId" crateNewDefCat)

checkExistId conn table checkname where' [value] ifTrue ifFalse = do
  check <- isExistInDb' conn table checkname where' [value]
  case check of
    True  -> ifTrue
    False -> do
      putStrLn $ checkname ++ ": " ++ unpack value ++ " doesn`t exist"
      ifFalse

parseConfCommLimit :: C.Config -> IO Integer
parseConfCommLimit conf = do
  str <- ((C.lookup conf "LimitNumbers.commentNumberLimit") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> inputInteger "commentNumberLimit"
    Just x  -> return x

parseConfDraftsLimit :: C.Config -> IO Integer
parseConfDraftsLimit conf = do
  str <- ((C.lookup conf "LimitNumbers.draftNumberLimit") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> inputInteger "draftNumberLimit"
    Just x  -> return x

parseConfPostsLimit :: C.Config -> IO Integer
parseConfPostsLimit conf = do
  str <- ((C.lookup conf "LimitNumbers.postNumberLimit") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> inputInteger "postNumberLimit"
    Just x  -> return x

inputInteger :: String -> IO Integer
inputInteger valueName = do
  putStrLn $ "Can`t parse value \"" ++ valueName ++ "\" from configuration file or command line\nPlease, enter number of " ++ valueName
  input <- getLine
  case reads input of
    [(a,"")] -> return a
    _        -> inputInteger valueName

inputIntegerOr :: String -> IO Integer -> IO Integer
inputIntegerOr valueName action = do
  putStrLn $ "Can`t parse value \"" ++ valueName ++ "\" from configuration file or command line\nPlease, enter number of " ++ valueName ++ "\nOr enter  NEW  to create a new " ++ valueName 
  input <- getLine
  case input of
    "NEW" -> action
    _     -> case reads input of
      [(a,"")] -> return a
      _        -> inputIntegerOr valueName action

crateNewDefPic :: IO Integer
crateNewDefPic = do
  defPicUrl <- getDefPicUrl
  picId <- createDefaultPicture defPicUrl
  putStrLn $ "Default picture created, id:" ++ show picId
  return picId

getDefPicUrl :: IO Text
getDefPicUrl = do
  putStrLn $ "Enter default picture url"
  input <- getLine
  (do
    res <- HT.httpLBS . fromString $ input  
    let bs = HT.getResponseBody res
    case decodeImage $ BSL.toStrict bs of
      Right _ -> return $ pack input
      Left _  -> do
        putStrLn $ "Invalid picture url:" ++ input
        getDefPicUrl) `catch` (\e -> do
          putStrLn $ "Invalid picture url:" ++ input ++ ". " ++ (show (e :: HT.HttpException))
          getDefPicUrl)

createDefaultPicture :: Text -> IO Integer
createDefaultPicture defPicUrl = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  [Only picId] <- query conn "INSERT INTO pics ( pic_url ) VALUES (?) RETURNING pic_id " [defPicUrl]
  return picId

crateNewDefUser :: Integer -> IO Integer
crateNewDefUser picId = do
  userId <- createDefaultUser picId
  putStrLn $ "Default user created, id:" ++ show userId
  return userId

createDefaultUser picId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  day <- App.getDay'  
  [Only userId] <- query conn "INSERT INTO users ( password, first_name , last_name , user_pic_id , user_create_date, admin) VALUES ( '12345678','DELETED','DELETED',?,?, false ) RETURNING user_id" [ pack (show picId), pack day ]
  return userId

crateNewDefAuthor :: Integer -> IO Integer
crateNewDefAuthor userId = do
  authorId <- createDefaultAuthor userId
  putStrLn $ "Default author created, id:" ++ show authorId
  return authorId

createDefaultAuthor userId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  [Only authorId] <- query conn "INSERT INTO authors ( user_id , author_info) VALUES ( ?,'DELETED' ) RETURNING author_id" [pack . show $ userId]  
  return authorId

crateNewDefCat :: IO Integer
crateNewDefCat = do
  catId <- createDefaultCategory
  putStrLn $ "Default category created, id:" ++ show catId
  return catId

createDefaultCategory :: IO Integer
createDefaultCategory = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  [Only catId] <- query_ conn "INSERT INTO categories (category_name) VALUES ( 'NONE' ) RETURNING category_id" 
  return catId


addCreateAdminKey = do
  conn1 <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn1 "INSERT INTO key (create_admin_key) VALUES ( 'lola' ) "
