module Conf.ConnectDB
  ( tryConnect,
    ConnDB (..),
    ConnectInfo (..),
    inputString,
    inputNum,
  )
where

import qualified Control.Exception as E
import Control.Monad.Catch (catch)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, connect)

data ConnDB = ConnDB Connection ConnectInfo

tryConnect :: ConnectInfo -> IO ConnDB
tryConnect connInf =
  ( do
      conn <- connect connInf
      return $ ConnDB conn connInf
  )
    `catch` ( \e -> do
                putStrLn $
                  "Can`t connect to database. ConnectInfo: " ++ show connInf
                    ++ ". "
                    ++ show (e :: E.IOException)
                connInf2 <- inputConnectInfo
                tryConnect connInf2
            )

inputConnectInfo :: IO ConnectInfo
inputConnectInfo = do
  hostDB <- inputString "DataBase.host"
  portDBInteger <- inputNum "DataBase.port"
  let portDB = fromInteger portDBInteger
  userDB <- inputString "DataBase.user"
  dbName <- inputString "DataBase.dbname"
  pwdDB <- inputString "DataBase.password"
  return (ConnectInfo hostDB portDB userDB dbName pwdDB)

inputNum :: (Num a, Read a) => String -> IO a
inputNum valueName = do
  putStrLn $
    "Can`t parse value \"" ++ valueName
      ++ "\" from configuration file or command line\nPlease, enter number of "
      ++ valueName
  input <- getLine
  case reads input of
    [] -> do
      putStrLn "Empty input"
      inputNum valueName
    [(a, "")] -> return a
    _ -> inputNum valueName

inputString :: String -> IO String
inputString valueName = do
  putStrLn $
    "Can`t parse value \"" ++ valueName
      ++ "\" from configuration file or command line\nPlease, enter "
      ++ valueName
  getLine
