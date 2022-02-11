{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}


module LoggerTest where

import Logger 
import TypesTest
import TestDB (TestDB)
import           Control.Monad.State  (StateT(..))          

handLogDebug :: LogHandle (StateT (TestDB,[MockAction]) IO)
handLogDebug = LogHandle (LogConfig DEBUG) logTest

logTest :: Priority -> String -> StateT (TestDB,[MockAction]) IO ()
logTest prio _ = StateT $ \(db,acts) -> 
  return (() , (db,LOG prio : acts))







