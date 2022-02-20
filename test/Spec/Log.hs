{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Log where

import Control.Monad.State (StateT (..))
import Logger
import Spec.TestDB (TestDB)
import Spec.Types

handLogDebug :: LogHandle (StateT (TestDB, [MockAction]) IO)
handLogDebug = LogHandle (LogConfig DEBUG) logTest

logTest :: Priority -> String -> StateT (TestDB, [MockAction]) IO ()
logTest prio _ = StateT $ \(db, acts) ->
  return ((), (db, LOG prio : acts))
