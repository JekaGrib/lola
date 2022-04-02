{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

import Spec.Admin (testAdmin)
import Spec.Auth (testAuth)
import Spec.Author (testAuthor)
import Spec.Category (testCat)
import Spec.Comment (testComm)
import Spec.Draft (testDraft)
import Spec.MakeCatResp (testMakeCatResp)
import Spec.Picture (testPic)
import Spec.Post (testPost)
import Spec.Tag (testTag)
import Spec.User (testUser)

main :: IO ()
main = do
  testAdmin
  testAuth
  testAuthor
  testCat
  testComm
  testDraft
  testMakeCatResp
  testPic
  testPost
  testTag
  testUser
