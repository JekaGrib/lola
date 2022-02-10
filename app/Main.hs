{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}




module Main where



import           App (application)
import           Logger (LogHandle(..),LogConfig(..),logger)
import Conf (parseConf,getTime,Config(cPriority),extractSettings)
import           Network.Wai.Handler.Warp       ( runSettings)



   
  

main :: IO ()
main = do
  time <- getTime                          
  let currLogPath = "./PostApp.LogSession: " ++ show time ++ " .log"
  config <- parseConf
  let sets = extractSettings config 
  let handleLog = LogHandle (LogConfig (cPriority config)) (logger handleLog currLogPath)
  runSettings sets (application config handleLog)



