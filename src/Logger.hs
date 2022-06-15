module Logger where

import Control.Monad (when)
import Prelude hiding (log)

data LogHandle m = LogHandle
  { hLogConf :: LogConfig,
    log :: Priority -> String -> m ()
  }

newtype LogConfig = LogConfig
  { cLogLevel :: Priority
  }

data Priority
  = DEBUG
  | INFO
  | WARNING
  | ERROR
  deriving (Ord, Eq, Show, Read)

logger :: String -> Priority -> String -> IO ()
logger logPath currentPrio str = do
  putStrLn (show currentPrio ++ ": " ++ str)
  appendFile logPath (show currentPrio ++ ": " ++ str ++ "\n")

checkPrioAndLog :: (Applicative m) => LogHandle m -> Priority -> String -> m ()
checkPrioAndLog h prio = when (prio >= configPrio) . log h prio
  where
    configPrio = cLogLevel (hLogConf h)

logDebug, logInfo, logWarning, logError :: (Applicative m) => LogHandle m -> String -> m ()
logDebug h = checkPrioAndLog h DEBUG
logInfo h = checkPrioAndLog h INFO
logWarning h = checkPrioAndLog h WARNING
logError h = checkPrioAndLog h ERROR
