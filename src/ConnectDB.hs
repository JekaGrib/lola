{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}




module ConnectDB (tryConnect,ConnDB(..),inputString,inputInteger) where
          

import           Database.PostgreSQL.Simple (connectPostgreSQL,Connection)
import qualified Control.Exception              as E
import           Data.String                    ( fromString )
import           Control.Monad.Catch            ( catch)


data ConnDB = ConnDB {hostCDB :: String, portCDB :: Integer, userCDB :: String, nameCDB :: String, pwdCDB :: String} 
   

tryConnect :: ConnDB -> IO (Connection, ConnDB)
tryConnect connDB@(ConnDB hostDB portDB userDB dbName pwdDB) = do
  let str = "host='" ++ hostDB ++ "' port=" ++ show portDB ++ " user='" ++ userDB ++ "' dbname='" ++ dbName ++ "' password='" ++ pwdDB ++ "'"
  (do 
    conn <- connectPostgreSQL (fromString str) 
    return (conn,connDB)) `catch` (\e -> do
      putStrLn $ "Can`t connect to database. Connection parameters: " ++ str ++ ". " ++ show (e :: E.IOException)
      connDB2 <- getConnDBParams
      tryConnect connDB2)

getConnDBParams :: IO ConnDB
getConnDBParams = do
  hostDB          <- inputString  "DataBase.host"
  portDB          <- inputInteger "DataBase.port"
  userDB          <- inputString  "DataBase.user"
  dbName          <- inputString  "DataBase.dbname"
  pwdDB           <- inputString  "DataBase.password"
  return (ConnDB hostDB portDB userDB dbName pwdDB)

inputInteger :: String -> IO Integer
inputInteger valueName = do
  putStrLn $ "Can`t parse value \"" ++ valueName ++ "\" from configuration file or command line\nPlease, enter number of " ++ valueName
  input <- getLine
  case reads input of
    [(a,"")] -> return a
    _        -> inputInteger valueName

inputString :: String -> IO String
inputString valueName = do
  putStrLn $ "Can`t parse value \"" ++ valueName ++ "\" from configuration file or command line\nPlease, enter " ++ valueName
  getLine

