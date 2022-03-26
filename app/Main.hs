{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import App (application)
import Conf (Config (cPriority), extractSettings, getTime, parseConf)
import Logger (LogConfig (..), LogHandle (..), logger)
import Network.Wai.Handler.Warp (runSettings)

main :: IO ()
main = do
  time <- getTime
  let currLogPath = "./PostApp.LogSession: " ++ show time ++ " .log"
  config <- parseConf
  let sets = extractSettings config
  let handleLog = LogHandle (LogConfig (cPriority config)) (logger currLogPath)
  runSettings sets (application config handleLog)
