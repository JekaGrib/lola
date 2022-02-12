{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module Conf where

import Conf.CreateDefault (createNewDefPic, createNewDefUser, createNewDefAuthor, createNewDefCat)
import Conf.ConnectDB (tryConnect,ConnDB(..),ConnectInfo(..),inputString,inputNum)
import Types
import           Logger
import           Data.Text                      ( Text, pack, unpack, intercalate )
import           Database.PostgreSQL.Simple (Connection,Only(..),query)
import           Data.Time.LocalTime (getZonedTime)
import qualified Data.Configurator              as C
import qualified Data.Configurator.Types        as C
import qualified Control.Exception              as E
import           Data.Char                      ( toUpper )
import Methods.Common.ToQuery (toExQ)
import Data.String (fromString)
import Network.Wai.Handler.Warp (defaultSettings,setHost,setPort,Settings)
import GHC.Word (Word16)

data Config = Config
  { cServHost    :: String,
    cServPort    :: Int,
    cConnDB      :: ConnDB,
    cPriority :: Priority,    
    cDefPicId    :: PictureId,
    cDefUsId     :: UserId,
    cDefAuthId   :: AuthorId,
    cDefCatId    :: CategoryId,
    cCommLimit   :: Limit,
    cDraftsLimit :: Limit,
    cPostsLimit  :: Limit
    }


getTime :: IO String
getTime = do
  time    <- getZonedTime
  return $ show time     

extractConn :: Config -> Connection
extractConn conf = let ConnDB conn _ = cConnDB conf in conn

extractSettings :: Config -> Settings
extractSettings conf = 
  let port = cServPort conf
      servH = fromString . cServHost $ conf
  in  setHost servH . setPort port $ defaultSettings

reConnectDB :: Config -> IO Config
reConnectDB oldConf = do
  let ConnDB _ connInfo = cConnDB oldConf
  connDB <- tryConnect connInfo
  return $ oldConf {cConnDB = connDB}

parseConf :: IO Config
parseConf = do
  conf            <- pullConfig
  servHost        <- parseConfServHost    conf
  servPort        <- parseConfServPort    conf
  hostDB          <- parseConfDBHost      conf     
  portDB          <- parseConfDBport      conf
  userDB          <- parseConfDBUser      conf
  dbName          <- parseConfDBname      conf
  pwdDB           <- parseConfDBpwd       conf
  connDB@(ConnDB conn _) <- tryConnect (ConnectInfo hostDB portDB userDB pwdDB dbName ) 
  defPicId        <- parseConfDefPicId    conf conn 
  defUsId         <- parseConfDefUsId     conf conn defPicId
  defAuthId       <- parseConfDefAuthId   conf conn defUsId
  defCatId        <- parseConfDefCatId    conf conn
  commNumLimit    <- parseConfCommLimit   conf
  draftsNumLimit  <- parseConfDraftsLimit conf
  postsNumLimit   <- parseConfPostsLimit  conf
  prio            <- parseConfPrio     conf 
  return $ Config (fromString servHost) servPort connDB prio defPicId defUsId defAuthId defCatId commNumLimit draftsNumLimit postsNumLimit 

pullConfig :: IO C.Config
pullConfig = do
  C.load [C.Required "./postApp.config"] 
    `E.catch` (\e -> putStrLn (show (e :: C.ConfigError)) >> return C.empty)
    `E.catch` (\e -> putStrLn (show (e :: C.KeyError   )) >> return C.empty)
    `E.catch` (\e -> putStrLn (show (e :: E.IOException  )) >> return C.empty)

parseConfServHost :: C.Config -> IO String
parseConfServHost conf = do
  str <- ((C.lookup conf "Server.host") :: IO (Maybe String))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe String) ) 
  case str of
    Nothing -> inputString "Server.host"
    Just x  -> return x

parseConfServPort :: C.Config -> IO Port
parseConfServPort conf = do
  str <- ((C.lookup conf "Server.port") :: IO (Maybe Port))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Port) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Port) ) 
  case str of
    Nothing -> inputNum "Server.port"
    Just x  -> return x

parseConfDBHost :: C.Config -> IO String
parseConfDBHost conf = do
  str <- ((C.lookup conf "DataBase.host") :: IO (Maybe String))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe String) ) 
  case str of
    Nothing -> inputString "DataBase.host"
    Just x  -> return x

parseConfDBport :: C.Config -> IO Word16
parseConfDBport conf = do
  str <- ((C.lookup conf "DataBase.port") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> inputNum "DataBase.port"
    Just x  -> return (fromInteger x)

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

parseConfCommLimit :: C.Config -> IO Limit
parseConfCommLimit conf = do
  str <- ((C.lookup conf "LimitNumbers.commentNumberLimit") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> do
      num <- inputNum "commentNumberLimit"
      checkLimitOr num (inputNum "commentNumberLimit")
    Just num  -> checkLimitOr num (inputNum "commentNumberLimit")

parseConfDraftsLimit :: C.Config -> IO Limit
parseConfDraftsLimit conf = do
  str <- ((C.lookup conf "LimitNumbers.draftNumberLimit") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> do
      num <- inputNum "draftNumberLimit"
      checkLimitOr num (inputNum "draftNumberLimit")
    Just num  -> checkLimitOr num (inputNum "draftNumberLimit")

parseConfPostsLimit :: C.Config -> IO Limit
parseConfPostsLimit conf = do
  str <- ((C.lookup conf "LimitNumbers.postNumberLimit") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> do
      num <- inputNum "postNumberLimit"
      checkLimitOr num (inputNum "postNumberLimit")
    Just num  -> checkLimitOr num (inputNum "postNumberLimit")

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
  putStrLn $ "Can`t parse value \"" ++ valueName ++ "\" from configuration file or command line\nPlease, enter number of " ++ valueName ++ "\nOr enter  `NEW`  to create a new " ++ valueName 
  input <- getLine
  case (map toUpper input) of
    "NEW" -> action
    _     -> case reads input of
      [(a,"")] -> return a
      _        -> inputIntegerOr valueName action

parseConfDefPicId :: C.Config -> Connection -> IO Integer
parseConfDefPicId conf conn = do
  str <- ((C.lookup conf "defaultValues.defaultPictureId") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> do
      x <- inputIntegerOr "defaultPictureId" (createNewDefPic conn)
      checkExistId conn "pics" "pic_id" "pic_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultPictureId" (createNewDefPic conn))
    Just x  -> do
      checkExistId conn "pics" "pic_id" "pic_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultPictureId" (createNewDefPic conn))

parseConfDefUsId :: C.Config -> Connection -> Integer -> IO Integer
parseConfDefUsId conf conn defPicId = do
  str <- ((C.lookup conf "defaultValues.defaultUserId") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> do
      x <- inputIntegerOr "defaultUserId" (createNewDefUser conn defPicId)
      checkExistId conn "users" "user_id" "user_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultUserId" (createNewDefUser conn defPicId))
    Just x  -> do
      checkExistId conn "users" "user_id" "user_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultUserId" (createNewDefUser conn defPicId))

parseConfDefAuthId :: C.Config -> Connection -> Integer -> IO Integer
parseConfDefAuthId conf conn defUsId = do
  str <- ((C.lookup conf "defaultValues.defaultAuthorId") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> do
      x <- inputIntegerOr "defaultAuthorId" (createNewDefAuthor conn defUsId)
      checkExistId conn "authors" "author_id" "author_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultAuthorId" (createNewDefAuthor conn defUsId))
    Just x  -> do
      checkExistId conn "authors" "author_id" "author_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultAuthorId" (createNewDefAuthor conn defUsId))

parseConfDefCatId :: C.Config -> Connection -> IO Integer
parseConfDefCatId conf conn = do
  str <- ((C.lookup conf "defaultValues.defaultCategoryId") :: IO (Maybe Integer))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Integer) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer) ) 
  case str of
    Nothing -> do
      x <- inputIntegerOr "defaultCategoryId" (createNewDefCat conn)
      checkExistId conn "categories" "category_id" "category_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultCategoryId" (createNewDefCat conn))
    Just x  -> do
      checkExistId conn "categories" "category_id" "category_id=?" [pack . show $ x] 
        (return x) 
        (inputIntegerOr "defaultCategoryId" (createNewDefCat conn))

checkExistId :: Connection -> String -> String -> String -> [Text] -> IO b -> IO b -> IO b
checkExistId conn table checkname where' values ifTrue ifFalse = do
  onlyChecks  <- query conn (toExQ table checkname where') values
  case onlyChecks of
    [Only True]  -> ifTrue
    [Only False] -> do
      putStrLn $ checkname ++ ": " ++ (unpack . intercalate "; " $ values) ++ " doesn`t exist"
      ifFalse
    _ -> do
      putStrLn $ "Something in DB went wrong with " ++ checkname ++ ": " ++ (unpack . intercalate "; " $ values)
      ifFalse

checkLimitOr :: Integer -> IO Integer -> IO Page
checkLimitOr num action
  | num <= 0 = do
    putStrLn "Limit should be greater then 0"
    newNum <- action
    checkLimitOr newNum action
  | num > 100 = do
    putStrLn  "Limit should be less then 100"
    newNum <- action
    checkLimitOr newNum action
  | otherwise = return (fromInteger num)