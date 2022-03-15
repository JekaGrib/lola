{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

import Spec.Auth
import Spec.Tag

main :: IO ()
main = do
  testAuth
  testTag
