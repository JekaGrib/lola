{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

import Spec.Auth (testAuth)
import Spec.Tag (testTag)
import Spec.Picture (testPic)

main :: IO ()
main = do
  testAuth
  testPic
  testTag
