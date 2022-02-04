{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}




module Main where



import           App
import           Logger
import ConnectDB (tryConnect,ConnDB(..),inputString,inputInteger)
import           Data.Text                      ( Text, pack, unpack, intercalate )
import           Network.Wai.Handler.Warp       ( run )
import           Database.PostgreSQL.Simple
import qualified Network.HTTP.Simple            as HT
import           Data.Time.LocalTime
import           Data.String                    ( fromString )
import           Control.Monad.Catch            ( catch )
import qualified Data.Configurator              as C
import qualified Data.Configurator.Types        as C
import qualified Control.Exception              as E
import qualified Data.ByteString.Lazy           as BSL
import           Codec.Picture                  ( decodeImage )
import           Data.Char                      ( toUpper )

pullConfig :: IO C.Config
pullConfig = do
  C.load [C.Required "./postApp.config"] 
    `E.catch` (\e -> putStrLn (show (e :: C.ConfigError)) >> return C.empty)
    `E.catch` (\e -> putStrLn (show (e :: C.KeyError   )) >> return C.empty)
    `E.catch` (\e -> putStrLn (show (e :: E.IOException  )) >> return C.empty)
    



getTime :: IO String
getTime = do
  time    <- getZonedTime
  return $ show time     
   
  

main :: IO ()
main = do
  time <- getTime                          
  let currLogPath = "./PostApp.LogSession: " ++ show time ++ " .log"
  conf            <- pullConfig
  hostDB          <- parseConfDBHost      conf     
  portDB          <- parseConfDBport      conf
  userDB          <- parseConfDBUser      conf
  dbName          <- parseConfDBname      conf
  pwdDB           <- parseConfDBpwd       conf
  (conn,connDB)   <- tryConnect (ConnDB hostDB portDB userDB dbName pwdDB) 
  defPicId        <- parseConfDefPicId    conf conn 
  defUsId         <- parseConfDefUsId     conf conn defPicId
  defAuthId       <- parseConfDefAuthId   conf conn defUsId
  defCatId        <- parseConfDefCatId    conf conn
  commNumLimit    <- parseConfCommLimit   conf
  draftsNumLimit  <- parseConfDraftsLimit conf
  postsNumLimit   <- parseConfPostsLimit  conf
  prio            <- parseConfPrio     conf 
  let handleLog = LogHandle (LogConfig prio) (logger handleLog currLogPath)
  let config = Config connDB defPicId defUsId defAuthId defCatId commNumLimit draftsNumLimit postsNumLimit
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

checkExistId :: Connection -> String -> String -> String -> [Text] -> IO b -> IO b -> IO b
checkExistId conn table checkname where' values ifTrue ifFalse = do
  check <- isExistInDb' conn table checkname where' values
  case check of
    True  -> ifTrue
    False -> do
      putStrLn $ checkname ++ ": " ++ (unpack . intercalate "; " $ values) ++ " doesn`t exist"
      ifFalse


parseConfDBHost :: C.Config -> IO String
parseConfDBHost conf = do
  str <- ((C.lookup conf "DataBase.host") :: IO (Maybe String))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe String) ) 
  case str of
    Nothing -> inputString "DataBase.host"
    Just x  -> return x

parseConfDBport :: C.Config -> IO Integer
parseConfDBport conf = do
  str <- ((C.lookup conf "DataBase.port") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> inputInteger "DataBase.port"
    Just x  -> return x

parseConfDBUser :: C.Config -> IO String
parseConfDBUser conf = do
  str <- ((C.lookup conf "DataBase.user") :: IO (Maybe String))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe String) ) 
  case str of
    Nothing -> inputString "DataBase.user"
    Just x  -> return x

parseConfDBname :: C.Config -> IO String
parseConfDBname conf = do
  str <- ((C.lookup conf "DataBase.dbname") :: IO (Maybe String))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe String) ) 
  case str of
    Nothing -> inputString "DataBase.dbname"
    Just x  -> return x

parseConfDBpwd :: C.Config -> IO String
parseConfDBpwd conf = do
  str <- ((C.lookup conf "DataBase.password") :: IO (Maybe String))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe String) ) 
  case str of
    Nothing -> inputString "DataBase.password"
    Just x  -> return x

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

parseConfPrio :: C.Config -> IO Priority
parseConfPrio conf = do
  str <- (C.lookup conf "log.logLevel" :: IO (Maybe String))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe String) ) 
  case str of
    Nothing -> inputLogLevel
    Just "DEBUG"   -> return DEBUG
    Just "INFO"    -> return INFO
    Just "WARNING" -> return WARNING
    Just "ERROR"   -> return ERROR
    Just _         -> inputLogLevel

inputLogLevel :: IO Priority
inputLogLevel = do
  putStrLn "Can`t parse value \"logLevel\" from configuration file or command line\nPlease, enter logging level (logs of this level and higher will be recorded)\nAvailable levels: DEBUG ; INFO ; WARNING ; ERROR (without quotes)"
  input <- getLine
  case (map toUpper input) of
    "DEBUG"   -> return DEBUG
    "INFO"    -> return INFO
    "WARNING" -> return WARNING
    "ERROR"   -> return ERROR
    _         -> inputLogLevel



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


createDefaultUser :: Integer -> IO Integer
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

createDefaultAuthor :: Integer -> IO Integer
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


addCreateAdminKey :: IO ()
addCreateAdminKey = do
  conn1 <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  _ <- execute_ conn1 "INSERT INTO key (create_admin_key) VALUES ( 'lola' ) "
  return ()
