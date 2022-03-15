{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Auth.QStrExample where

import Spec.Conf (defConf)
import Control.Monad.Catch (SomeException, catch, throwM)
import Control.Monad.State (StateT (..), withStateT)
import Data.Text (Text, unpack)
import Spec.Log (handLogDebug)
import Methods.Tag
import Spec.Oops (UnexpectedArgsException (..))
import Spec.TestDB
import Types
import Spec.Types (MockAction (..))
import Methods.Common.Auth (Handle(..))
import Network.HTTP.Types (QueryText)

qStr1 :: QueryText
qStr1 = [("token",Just "lola")]



