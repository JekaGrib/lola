module Spec.Conf where

import Conf
import Conf.ConnectDB (ConnDB (..), ConnectInfo (..))
import Database.PostgreSQL.Simple (Connection, connect)
import Logger (Priority (..))
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE getEmptyConn #-}
getEmptyConn :: ConnectInfo -> Connection
getEmptyConn connInfo = unsafePerformIO $ connect connInfo

defConf :: Config
defConf = Config "localhost" 3000 emptyConnDB DEBUG 1 1 1 1 20 5 5

emptyConnDB :: ConnDB
emptyConnDB = ConnDB (getEmptyConn emptyConnectInfo) emptyConnectInfo

emptyConnectInfo :: ConnectInfo
emptyConnectInfo = ConnectInfo "" 0 "" "" ""
