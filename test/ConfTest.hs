{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}


module ConfTest where

import Logger (Priority(..))
import Conf.ConnectDB (ConnDB(..),ConnectInfo(..))
import           Database.PostgreSQL.Simple (connect,Connection)
import           Conf
import System.IO.Unsafe (unsafePerformIO)

defConf :: Config
defConf = Config "localhost" 3000 emptyConnDB DEBUG 1 1 1 1 20 5 5 

emptyConnDB :: ConnDB
emptyConnDB = ConnDB emptyConn emptyConnectInfo

emptyConnectInfo :: ConnectInfo
emptyConnectInfo = ConnectInfo "" 0 "" "" ""

emptyConn :: Connection
emptyConn = unsafePerformIO $ connect emptyConnectInfo





