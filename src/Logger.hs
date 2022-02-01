{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module Logger where

import           Prelude          hiding (log)



data LogHandle m = LogHandle 
   { hLogConf :: LogConfig,
     log      :: Priority -> String -> m ()}

data LogConfig = LogConfig
  { cLogLevel :: Priority }

data Priority = DEBUG | INFO | WARNING | ERROR 
                                          deriving (Ord,Eq,Show)

logger :: LogHandle IO -> String -> Priority -> String -> IO ()
logger h logPath currP str  
    | currP >= configP = do
        appendFile logPath (show currP ++ ": " ++ str ++ "\n")
    | otherwise        = return ()
      where configP = cLogLevel (hLogConf h)

logDebug, logInfo, logWarning, logError :: LogHandle m -> String -> m ()
logDebug   h = log h DEBUG
logInfo    h = log h INFO
logWarning h = log h WARNING
logError   h = log h ERROR



