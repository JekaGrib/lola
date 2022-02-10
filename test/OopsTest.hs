{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}




module OopsTest where

import           Control.Monad.Catch            ( Exception)



data UnexpectedArgsException = UnexpectedArgsException
  deriving (Eq,Show)
  

instance Exception UnexpectedArgsException





