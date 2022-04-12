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
  putStrLn "Test Methods.Admin.hs"
  testAdmin
  putStrLn "Test Methods.Auth.hs"
  testAuth
  putStrLn "Test Methods.Author.hs"
  testAuthor
  putStrLn "Test Methods.Category.hs"
  testCat
  putStrLn "Test Methods.Comment.hs"
  testComm
  putStrLn "Test Methods.Draft.hs"
  testDraft
  putStrLn "Test Methods.Common.MakeCatResp.hs"
  testMakeCatResp
  putStrLn "Test Methods.Picture.hs"
  testPic
  putStrLn "Test Methods.Post.hs"
  testPost
  putStrLn "Test Methods.Tag.hs"
  testTag
  putStrLn "Test Methods.User.hs"
  testUser
