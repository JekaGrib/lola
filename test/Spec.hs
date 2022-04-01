{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

import Spec.Auth (testAuth)
import Spec.Author (testAuthor)
import Spec.Tag (testTag)
import Spec.Picture (testPic)
import Spec.User (testUser)
import Spec.MakeCatResp (testMakeCatResp)
import Spec.Category (testCat)
import Spec.Comment (testComm)
import Spec.Draft (testDraft)

main :: IO ()
main = do
  testAuth
  testAuthor
  testCat
  testComm
  testDraft
  testMakeCatResp
  testPic
  testTag
  testUser