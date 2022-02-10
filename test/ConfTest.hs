{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}

module ConfTest where

import           Conf
import System.IO.Unsafe (unsafePerformIO)

emptyConnectInfo = ConnectInfo "" 0 "" "" ""

emptyConn = unsafePerformIO $ connect emptyConnectInfo

emptyConnDB = ConnDB emptyConn emptyConnDBInfo

defConf = Config "localhost" 3000 emptyConnDB DEBUG 1 1 1 1 20 5 5 

