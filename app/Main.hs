{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}




module Main where



import           App (application)
import           Logger (LogHandle(..),LogConfig(..),logger)
import Conf (parseConf,getTime,Config(cPriority))
import           Network.Wai.Handler.Warp       ( run )



   
  

main :: IO ()
main = do
  time <- getTime                          
  let currLogPath = "./PostApp.LogSession: " ++ show time ++ " .log"
  config <- parseConf 
  let handleLog = LogHandle (LogConfig (cPriority config)) (logger handleLog currLogPath)
  run 3000 (application config handleLog)



