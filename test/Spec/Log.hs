{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Log where

import Control.Monad.State (StateT (..),modify)
import Logger
import Spec.Types

handLogDebug :: LogHandle (StateT [MockAction] IO)
handLogDebug = LogHandle (LogConfig DEBUG) logTest

logTest :: Priority -> String -> StateT [MockAction] IO ()
logTest prio _ = modify (LOG prio :)

