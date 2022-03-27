{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Auth.Handlers where

import Control.Monad.State (StateT (..), modify)
import Methods.Common.Auth (Handle (..))
import Spec.Auth.Types
import Spec.Conf (defConf)
import Spec.Log (handLogDebug,handLogWarning)
import Spec.Types (MockAction (..))
import Types

handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogWarning
    selectTokenKeysForUserTest

handle0 :: Handle (StateT [MockAction] IO)
handle0 = handle {hLog = handLogDebug}

selectTokenKeysForUserTest :: UserId -> StateT [MockAction] IO [TokenKey]
selectTokenKeysForUserTest usId = do
  modify (AuthMock (SelectTokenKeyForUser usId) :)
  return ["lola"]
