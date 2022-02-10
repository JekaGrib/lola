{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}




module Conf.ConnectDB (tryConnect,ConnDB(..),ConnDBInfo(..),inputString,inputNum) where
          

import           Database.PostgreSQL.Simple (connectPostgreSQL,Connection)
import qualified Control.Exception              as E
import           Data.String                    ( fromString )
import           Control.Monad.Catch            ( catch)


data ConnDB = ConnDB Connection ConnDBInfo

data ConnDBInfo = ConnDBInfo {hostCDB :: String, portCDB :: Integer, userCDB :: String, nameCDB :: String, pwdCDB :: String} 



tryConnect :: ConnDBInfo -> IO ConnDB
tryConnect connDBInf@(ConnDBInfo hostDB portDB userDB dbName pwdDB) = do
  let str = "host='" ++ hostDB ++ "' port=" ++ show portDB ++ " user='" ++ userDB ++ "' dbname='" ++ dbName ++ "' password='" ++ pwdDB ++ "'"
  (do 
    conn <- connectPostgreSQL (fromString str) 
    return $ ConnDB conn connDBInf) `catch` (\e -> do
      putStrLn $ "Can`t connect to database. Connection parameters: " ++ str ++ ". " ++ show (e :: E.IOException)
      connDBInf2 <- inputConnDBInfo
      tryConnect connDBInf2)

inputConnDBInfo :: IO ConnDBInfo
inputConnDBInfo = do
  hostDB          <- inputString  "DataBase.host"
  portDB          <- inputNum "DataBase.port"
  userDB          <- inputString  "DataBase.user"
  dbName          <- inputString  "DataBase.dbname"
  pwdDB           <- inputString  "DataBase.password"
  return (ConnDBInfo hostDB portDB userDB dbName pwdDB)

inputNum :: (Num a,Read a) => String -> IO a
inputNum valueName = do
  putStrLn $ "Can`t parse value \"" ++ valueName ++ "\" from configuration file or command line\nPlease, enter number of " ++ valueName
  input <- getLine
  case reads input of
    [] ->  do 
      putStrLn "Empty input"
      inputNum valueName
    [(a,"")] -> return a
    _        -> inputNum valueName

inputString :: String -> IO String
inputString valueName = do
  putStrLn $ "Can`t parse value \"" ++ valueName ++ "\" from configuration file or command line\nPlease, enter " ++ valueName
  getLine


