module Conf where

import Conf.ConnectDB (ConnDB (..), ConnectInfo (..), inputNum, inputString, tryConnect)
import Conf.CreateDefault (createNewDefAuthor, createNewDefCat, createNewDefPic, createNewDefUser)
import qualified Control.Exception as E
import Control.Monad (when)
import Data.Char (toUpper)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.List ((\\))
import Data.String (fromString)
import Data.Time.LocalTime (getZonedTime)
import Database.PostgreSQL.Simple (Connection, Only (..), query)
import GHC.Word (Word16)
import Logger
import Network.Wai.Handler.Warp (Settings, defaultSettings, setHost, setPort)
import Psql.Migration (Migrate (..), migrate)
import Psql.ToQuery
import Psql.ToQuery.Exists
import Psql.ToQuery.Select
import Text.Read (readMaybe)
import Types

data Config = Config
  { cServerHost :: String,
    cServerPort :: Int,
    cConnDB :: ConnDB,
    cPriority :: Priority,
    cDefPicId :: PictureId,
    cDefUsId :: UserId,
    cDefAuthId :: AuthorId,
    cDefCatId :: CategoryId,
    cCommentLimit :: Limit,
    cDraftsLimit :: Limit,
    cPostsLimit :: Limit
  }

getTime :: IO String
getTime = show <$> getZonedTime

extractConn :: Config -> Connection
extractConn conf = let ConnDB conn _ = cConnDB conf in conn

extractSettings :: Config -> Settings
extractSettings conf =
  let port = cServerPort conf
      serverH = fromString . cServerHost $ conf
   in setHost serverH . setPort port $ defaultSettings

reConnectDB :: Config -> IO Config
reConnectDB oldConf = do
  let ConnDB _ connInfo = cConnDB oldConf
  connDB <- tryConnect connInfo
  return $ oldConf {cConnDB = connDB}

parseConfAnd :: Migrate -> IO Config
parseConfAnd migrateArg = do
  conf <- pullConfig
  serverHost <- parseConfServerHost conf
  serverPort <- parseConfServerPort conf
  hostDB <- parseConfDBHost conf
  portDB <- parseConfDBport conf
  userDB <- parseConfDBUser conf
  dbName <- parseConfDBname conf
  pwdDB <- parseConfDBpwd conf
  connDB@(ConnDB conn _) <- tryConnect (ConnectInfo hostDB portDB userDB pwdDB dbName)
  when (migrateArg == Migrate) $ migrate conn
  defPicId <- parseConfDefPicId conf conn
  defUsId <- parseConfDefUsId conf conn defPicId
  defAuthId <- parseConfDefAuthId conf conn defUsId
  defCatId <- parseConfDefCatId conf conn
  commentNumLimit <- parseConfCommentLimit conf
  draftsNumLimit <- parseConfDraftsLimit conf
  postsNumLimit <- parseConfPostsLimit conf
  prio <- parseConfPrio conf
  return $ Config (fromString serverHost) serverPort connDB prio defPicId defUsId defAuthId defCatId commentNumLimit draftsNumLimit postsNumLimit

pullConfig :: IO C.Config
pullConfig =
  C.load [C.Required "./postApp.config"]
    `E.catch` (\e -> print (e :: C.ConfigError) >> return C.empty)
    `E.catch` (\e -> print (e :: C.KeyError) >> return C.empty)
    `E.catch` (\e -> print (e :: E.IOException) >> return C.empty)

parseConfServerHost :: C.Config -> IO String
parseConfServerHost conf = do
  str <-
    (C.lookup conf "Server.host" :: IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe String))
  case str of
    Nothing -> inputString "Server.host"
    Just x -> return x

parseConfServerPort :: C.Config -> IO Port
parseConfServerPort conf = do
  str <-
    (C.lookup conf "Server.port" :: IO (Maybe Port))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe Port))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe Port))
  case str of
    Nothing -> inputNum "Server.port"
    Just x -> return x

parseConfDBHost :: C.Config -> IO String
parseConfDBHost conf = do
  str <-
    (C.lookup conf "DataBase.host" :: IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe String))
  case str of
    Nothing -> inputString "DataBase.host"
    Just x -> return x

parseConfDBport :: C.Config -> IO Word16
parseConfDBport conf = do
  str <-
    (C.lookup conf "DataBase.port" :: IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer))
  case str of
    Nothing -> inputNum "DataBase.port"
    Just x -> return (fromInteger x)

parseConfDBUser :: C.Config -> IO String
parseConfDBUser conf = do
  str <-
    (C.lookup conf "DataBase.user" :: IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe String))
  case str of
    Nothing -> inputString "DataBase.user"
    Just x -> return x

parseConfDBname :: C.Config -> IO String
parseConfDBname conf = do
  str <-
    (C.lookup conf "DataBase.dbname" :: IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe String))
  case str of
    Nothing -> inputString "DataBase.dbname"
    Just x -> return x

parseConfDBpwd :: C.Config -> IO String
parseConfDBpwd conf = do
  str <-
    (C.lookup conf "DataBase.password" :: IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe String))
  case str of
    Nothing -> inputString "DataBase.password"
    Just x -> return x

parseConfCommentLimit :: C.Config -> IO Limit
parseConfCommentLimit conf = do
  str <-
    (C.lookup conf "LimitNumbers.commentNumberLimit" :: IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer))
  case str of
    Nothing -> do
      num <- inputNum "commentNumberLimit"
      checkLimitOr num (inputNum "commentNumberLimit")
    Just num -> checkLimitOr num (inputNum "commentNumberLimit")

parseConfDraftsLimit :: C.Config -> IO Limit
parseConfDraftsLimit conf = do
  str <-
    (C.lookup conf "LimitNumbers.draftNumberLimit" :: IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer))
  case str of
    Nothing -> do
      num <- inputNum "draftNumberLimit"
      checkLimitOr num (inputNum "draftNumberLimit")
    Just num -> checkLimitOr num (inputNum "draftNumberLimit")

parseConfPostsLimit :: C.Config -> IO Limit
parseConfPostsLimit conf = do
  str <-
    (C.lookup conf "LimitNumbers.postNumberLimit" :: IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer))
  case str of
    Nothing -> do
      num <- inputNum "postNumberLimit"
      checkLimitOr num (inputNum "postNumberLimit")
    Just num -> checkLimitOr num (inputNum "postNumberLimit")

parseConfPrio :: C.Config -> IO Priority
parseConfPrio conf = do
  str <-
    (C.lookup conf "log.logLevel" :: IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe String))
  case fmap (map toUpper) str >>= readMaybe of
    Just p -> pure p
    _ -> inputLogLevel

inputLogLevel :: IO Priority
inputLogLevel = do
  putStrLn "Can`t parse value \"logLevel\" from configuration file or command line\nPlease, enter logging level (logs of this level and higher will be recorded)\nAvailable levels: DEBUG ; INFO ; WARNING ; ERROR (without quotes)"
  input <- getLine
  case readMaybe (map toUpper input) of
    Just p -> pure p
    _ -> inputLogLevel

inputIdOr :: String -> IO Id -> IO Id
inputIdOr valueName action = do
  putStrLn $ "Can`t parse value \"" ++ valueName ++ "\" from configuration file or command line\nPlease, enter number from 1 to 9223372036854775807\nOr enter  `NEW`  to create a new " ++ valueName
  input <- getLine
  case map toUpper input of
    "NEW" -> action
    _ -> case reads input of
      [(a, "")] ->
        checkBigIntOr a . inputIdOr valueName $ action
      _ -> inputIdOr valueName action

inputOrCreateDefCatId, inputOrCreateDefPicId :: Connection -> IO Id
inputOrCreateDefCatId conn = inputIdOr "defaultCategoryId" (createNewDefCat conn)
inputOrCreateDefPicId conn = inputIdOr "defaultPictureId" (createNewDefPic conn)

inputOrCreateDefUsId :: Connection -> PictureId -> IO UserId
inputOrCreateDefUsId conn defPicId = inputIdOr "defaultUserId" (createNewDefUser conn defPicId)

inputOrCreateDefAuthId :: Connection -> UserId -> IO AuthorId
inputOrCreateDefAuthId conn defUsId = inputIdOr "defaultAuthorId" (createNewDefAuthor conn defUsId)

parseConfDefPicId :: C.Config -> Connection -> IO PictureId
parseConfDefPicId conf conn = do
  str <-
    (C.lookup conf "defaultValues.defaultPictureId" :: IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer))
  case str of
    Nothing -> do
      iD <- inputOrCreateDefPicId conn
      checkExistId
        conn
        "pics"
        "pic_id=?"
        (Id iD)
        (return iD)
        (inputOrCreateDefPicId conn)
    Just x -> do
      iD <- checkBigIntOr x $ inputOrCreateDefPicId conn
      checkExistId
        conn
        "pics"
        "pic_id=?"
        (Id iD)
        (return iD)
        (inputOrCreateDefPicId conn)

parseConfDefUsId :: C.Config -> Connection -> PictureId -> IO UserId
parseConfDefUsId conf conn defPicId = do
  str <-
    (C.lookup conf "defaultValues.defaultUserId" :: IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer))
  case str of
    Nothing -> do
      iD <- inputOrCreateDefUsId conn defPicId
      checkExistId
        conn
        "users"
        "user_id=?"
        (Id iD)
        (return iD)
        (inputOrCreateDefUsId conn defPicId)
    Just x -> do
      iD <- checkBigIntOr x $ inputOrCreateDefUsId conn defPicId
      checkExistId
        conn
        "users"
        "user_id=?"
        (Id iD)
        (return iD)
        (inputOrCreateDefUsId conn defPicId)

parseConfDefAuthId :: C.Config -> Connection -> UserId -> IO AuthorId
parseConfDefAuthId conf conn defUsId = do
  str <-
    (C.lookup conf "defaultValues.defaultAuthorId" :: IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer))
  case str of
    Nothing -> do
      iD <- inputOrCreateDefAuthId conn defUsId
      checkExistId
        conn
        "authors"
        "author_id=?"
        (Id iD)
        (return iD)
        (inputOrCreateDefAuthId conn defUsId)
    Just x -> do
      iD <- checkBigIntOr x $ inputOrCreateDefAuthId conn defUsId
      checkExistId
        conn
        "authors"
        "author_id=?"
        (Id iD)
        (return iD)
        (inputOrCreateDefAuthId conn defUsId)

parseConfDefCatId :: C.Config -> Connection -> IO CategoryId
parseConfDefCatId conf conn = do
  str <-
    (C.lookup conf "defaultValues.defaultCategoryId" :: IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe Integer))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe Integer))
  case str of
    Nothing -> do
      iD <- inputOrCreateDefCatId conn
      checkExistId
        conn
        "categories"
        "category_id=?"
        (Id iD)
        (return iD)
        (inputOrCreateDefCatId conn)
    Just x -> do
      iD <- checkBigIntOr x $ inputOrCreateDefCatId conn
      checkExistId
        conn
        "categories"
        "category_id=?"
        (Id iD)
        (return iD)
        (inputOrCreateDefCatId conn)

checkExistId :: Connection -> String -> String -> DbValue -> IO Id -> IO Id -> IO Id
checkExistId conn table where' value ifTrue ifFalse = do
  let exi = Exists table (WherePair where' value)
  onlyChecks <- query conn (toQ exi) (toVal exi)
  case onlyChecks of
    [Only True] -> ifTrue
    [Only False] -> do
      putStrLn $ (where' \\ "=?") ++ ": " ++ show value ++ " doesn`t exist"
      ifFalse
    _ -> do
      putStrLn $ "Something in DB went wrong with " ++ (where' \\ "=?") ++ ": " ++ show value
      ifFalse

checkLimitOr :: Integer -> IO Integer -> IO Page
checkLimitOr num action
  | num <= 0 = do
    putStrLn "Limit should be greater then 0"
    newNum <- action
    checkLimitOr newNum action
  | num > 100 = do
    putStrLn "Limit should be less then 100"
    newNum <- action
    checkLimitOr newNum action
  | otherwise = return (fromInteger num)

checkBigIntOr :: Integer -> IO Id -> IO Id
checkBigIntOr num action
  | num <= 0 = do
    putStrLn "Id should be greater then 0"
    action
  | num > 9223372036854775807 = do
    putStrLn "Id should be less then 9223372036854775807"
    action
  | otherwise = return (fromInteger num)
