module Main where

import App (application)
import Conf (Config (cPriority), extractSettings, getTime, parseConfAnd)
import Logger (LogConfig (..), LogHandle (..), logger)
import Network.Wai.Handler.Warp (runSettings)
import Psql.Migration (readMigrateArg)

main :: IO ()
main = do
  time <- getTime
  mirateArg <- readMigrateArg
  config <- parseConfAnd mirateArg
  let currLogPath = "./PostApp.LogSession: " ++ show time ++ " .log"
  let handleLog = LogHandle (LogConfig (cPriority config)) (logger currLogPath)
  let sets = extractSettings config
  runSettings sets (application config handleLog)
