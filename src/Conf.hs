module Conf where

import Conf.ConnectDB (ConnDB (..), ConnectInfo (..), inputNum, inputString, tryConnect)
import Conf.CreateDefault
  ( createNewDefAuthor,
    createNewDefCat,
    createNewDefPic,
    createNewDefUser,
  )
import qualified Control.Exception as E
import Data.Char (toUpper)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.List ((\\))
import Data.String (fromString)
import Data.Text (unpack)
import Data.Time.LocalTime (getZonedTime)
import Database.PostgreSQL.Simple (Connection, Only (..), query)
import GHC.Word (Word16)
import Logger
import Network.Wai.Handler.Warp (Settings, defaultSettings, setHost, setPort)
import Psql.Migration (Migrate, migrateAll)
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
  let connInfo = ConnectInfo hostDB portDB userDB pwdDB dbName
  connDB@(ConnDB conn _) <- tryConnect connInfo
  migrateAll conn migrateArg
  defPicId <- parseConfDefPicId conf conn
  defUsId <- parseConfDefUsId conf conn defPicId
  defAuthId <- parseConfDefAuthId conf conn defUsId
  defCatId <- parseConfDefCatId conf conn
  commentNumLimit <- parseConfCommentLimit conf
  draftsNumLimit <- parseConfDraftsLimit conf
  postsNumLimit <- parseConfPostsLimit conf
  prio <- parseConfPrio conf
  return $
    Config
      (fromString serverHost)
      serverPort
      connDB
      prio
      defPicId
      defUsId
      defAuthId
      defCatId
      commentNumLimit
      draftsNumLimit
      postsNumLimit

pullConfig :: IO C.Config
pullConfig =
  C.load [C.Required "./postApp.config"]
    `E.catch` (\e -> print (e :: C.ConfigError) >> return C.empty)
    `E.catch` (\e -> print (e :: C.KeyError) >> return C.empty)
    `E.catch` (\e -> print (e :: E.IOException) >> return C.empty)

lookUpConf :: C.Configured a => C.Config -> C.Name -> IO (Maybe a)
lookUpConf conf name =
  C.lookup conf name
    `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe a))
    `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe a))

parseConf :: C.Configured a => C.Config -> C.Name -> IO a -> IO a
parseConf conf name orToDo =
  lookUpConf conf name >>= maybe orToDo pure

parseStingConf :: C.Config -> C.Name -> IO String
parseStingConf conf name = parseConf conf name $ inputString (unpack name)

parseNumConf :: (C.Configured a, Num a, Read a) => C.Config -> C.Name -> IO a
parseNumConf conf name = parseConf conf name $ inputNum (unpack name)

parseLimitConf :: C.Config -> C.Name -> IO Limit
parseLimitConf conf name =
  lookUpConf conf name >>= maybe orToDoIfNothing orToDoIfJust
  where
    orToDoIfJust num = checkLimitOr num $ inputNum (unpack name)
    orToDoIfNothing = do
      num <- inputNum (unpack name)
      orToDoIfJust num

parseConfServerHost :: C.Config -> IO String
parseConfServerHost conf = parseStingConf conf "Server.host"

parseConfServerPort :: C.Config -> IO Port
parseConfServerPort conf = parseNumConf conf "Server.port"

parseConfDBHost :: C.Config -> IO String
parseConfDBHost conf = parseStingConf conf "DataBase.host"

parseConfDBport :: C.Config -> IO Word16
parseConfDBport conf =
  fromInteger <$> parseNumConf conf "DataBase.port"

parseConfDBUser :: C.Config -> IO String
parseConfDBUser conf = parseStingConf conf "DataBase.user"

parseConfDBname :: C.Config -> IO String
parseConfDBname conf = parseStingConf conf "DataBase.dbname"

parseConfDBpwd :: C.Config -> IO String
parseConfDBpwd conf = parseStingConf conf "DataBase.password"

parseConfCommentLimit :: C.Config -> IO Limit
parseConfCommentLimit conf =
  parseLimitConf conf "LimitNumbers.commentNumberLimit"

parseConfDraftsLimit :: C.Config -> IO Limit
parseConfDraftsLimit conf =
  parseLimitConf conf "LimitNumbers.draftNumberLimit"

parseConfPostsLimit :: C.Config -> IO Limit
parseConfPostsLimit conf =
  parseLimitConf conf "LimitNumbers.postNumberLimit"

parseConfPrio :: C.Config -> IO Priority
parseConfPrio conf = do
  maybeStr <- lookUpConf conf "log.logLevel"
  case fmap (map toUpper) maybeStr >>= readMaybe of
    Just p -> pure p
    _ -> inputLogLevel

inputLogLevel :: IO Priority
inputLogLevel = do
  let str =
        "Can`t parse value \"logLevel\" from configuration\
        \ file or command line\nPlease, enter logging level (logs of\
        \ this level and higher will be recorded)\nAvailable levels:\
        \ DEBUG ; INFO ; WARNING ; ERROR (without quotes)"
  putStrLn str
  input <- getLine
  case readMaybe (map toUpper input) of
    Just p -> pure p
    _ -> inputLogLevel

inputIdOr :: String -> IO Id -> IO Id
inputIdOr valueName action = do
  putStrLn $
    "Can`t parse value \"" ++ valueName
      ++ "\" from configuration file or command line\n\
         \Please, enter number from 1 to 9223372036854775807\n\
         \Or enter  `NEW`  to create a new "
      ++ valueName
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
inputOrCreateDefUsId conn defPicId =
  inputIdOr "defaultUserId" (createNewDefUser conn defPicId)

inputOrCreateDefAuthId :: Connection -> UserId -> IO AuthorId
inputOrCreateDefAuthId conn defUsId =
  inputIdOr "defaultAuthorId" (createNewDefAuthor conn defUsId)

parseConfDefId ::
  Connection ->
  C.Config ->
  C.Name ->
  (Connection -> IO Id) ->
  (Connection -> Id -> IO Id) ->
  IO Id
parseConfDefId conn conf name inputOrCreateFunc checkExistFunc = do
  maybeInteger <- lookUpConf conf name
  case maybeInteger of
    Nothing -> do
      iD <- inputOrCreateFunc conn
      checkExistFunc conn iD
    Just num -> do
      iD <- checkBigIntOr num $ inputOrCreateFunc conn
      checkExistFunc conn iD

parseConfDefPicId :: C.Config -> Connection -> IO PictureId
parseConfDefPicId conf conn =
  parseConfDefId conn conf name inputOrCreateFunc checkExistFunc
  where
    name = "defaultValues.defaultPictureId"
    inputOrCreateFunc = inputOrCreateDefPicId
    checkExistFunc =
      checkExistId
        (inputOrCreateFunc conn)
        "pics"
        "pic_id=?"

parseConfDefUsId :: C.Config -> Connection -> PictureId -> IO UserId
parseConfDefUsId conf conn defPicId =
  parseConfDefId conn conf name inputOrCreateFunc checkExistFunc
  where
    name = "defaultValues.defaultUserId"
    inputOrCreateFunc conn' = inputOrCreateDefUsId conn' defPicId
    checkExistFunc =
      checkExistId
        (inputOrCreateFunc conn)
        "users"
        "user_id=?"

parseConfDefAuthId :: C.Config -> Connection -> UserId -> IO AuthorId
parseConfDefAuthId conf conn defUsId =
  parseConfDefId conn conf name inputOrCreateFunc checkExistFunc
  where
    name = "defaultValues.defaultAuthorId"
    inputOrCreateFunc conn' = inputOrCreateDefAuthId conn' defUsId
    checkExistFunc =
      checkExistId
        (inputOrCreateFunc conn)
        "authors"
        "author_id=?"

parseConfDefCatId :: C.Config -> Connection -> IO CategoryId
parseConfDefCatId conf conn =
  parseConfDefId conn conf name inputOrCreateFunc checkExistFunc
  where
    name = "defaultValues.defaultCategoryId"
    inputOrCreateFunc = inputOrCreateDefCatId
    checkExistFunc =
      checkExistId
        (inputOrCreateFunc conn)
        "categories"
        "category_id=?"

checkExistId :: IO Id -> Table -> String -> Connection -> Id -> IO Id
checkExistId ifFalse table where' conn iD = do
  let value = Id iD
  let exi = Exists table (WherePair where' value)
  onlyChecks <- query conn (toQ exi) (toVal exi)
  case onlyChecks of
    [Only True] -> return iD
    [Only False] -> do
      putStrLn $
        (where' \\ "=?") ++ ": " ++ show value
          ++ " doesn`t exist"
      ifFalse
    _ -> do
      putStrLn $
        "Something in DB went wrong with "
          ++ (where' \\ "=?")
          ++ ": "
          ++ show value
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
