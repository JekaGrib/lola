{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

import Spec.Auth (testAuth)
import Spec.Tag (testTag)
import Spec.Picture (testPic)
import Spec.User (testUser)
import Spec.MakeCatResp (testMakeCatResp)
import Spec.Category (testCat)

main :: IO ()
main = do
  testAuth
  testCat
  testMakeCatResp
  testPic
  testTag
  testUser