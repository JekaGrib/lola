{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Auth.Handlers where

import Spec.Conf (defConf)
import Control.Monad.Catch (SomeException, catch, throwM)
import Control.Monad.State (StateT (..), withStateT,modify)
import Data.Text (Text, unpack)
import Spec.Log (handLogDebug)
import Spec.Oops (UnexpectedArgsException (..))
import Spec.TestDB
import Types
import Spec.Types (MockAction (..))
import Methods.Common.Auth (Handle(..))
import Spec.Auth.Types

handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogDebug
    selectTokenKeysForUserTest


selectTokenKeysForUserTest :: UserId -> StateT [MockAction] IO [TokenKey]
selectTokenKeysForUserTest usId = do
  modify (AuthMock (SelectTokenKeyForUser usId) :)
  return ["lola"]



