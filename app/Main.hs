module Main where

import App (application)
import Conf (Config (cPriority), extractSettings, getTime, parseConfAnd)
import Logger (LogConfig (..), LogHandle (..), logger)
import Network.Wai.Handler.Warp (runSettings)
import Psql.Migration (Migrate (Migrate))

main :: IO ()
main = do
  time <- getTime
  let currLogPath = "./PostApp.LogSession: " ++ show time ++ " .log"
  config <- parseConfAnd Migrate
  let sets = extractSettings config
  let handleLog = LogHandle (LogConfig (cPriority config)) (logger currLogPath)
  runSettings sets (application config handleLog)
