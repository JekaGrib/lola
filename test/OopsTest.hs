{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module OopsTest where

import Control.Monad.Catch (Exception)

data UnexpectedArgsException = UnexpectedArgsException
  deriving (Eq, Show)

instance Exception UnexpectedArgsException
