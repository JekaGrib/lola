{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Exist.Types where

import Spec.Conf (defConf)
import Control.Monad.Catch (SomeException, catch, throwM)
import Control.Monad.State (StateT (..), withStateT)
import Data.Text (Text, unpack)
import Methods.Tag
import Methods.Common.Exist.UncheckedExId (UncheckedExId)
import Spec.Oops (UnexpectedArgsException (..))
import Spec.TestDB
import Types
import Methods.Common.Auth (Handle(..))

data ExistMock = IsExist UncheckedExId
  deriving (Eq, Show)



