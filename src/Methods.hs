--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}

module Methods where

import Conf (Config)
import Logger (LogHandle)
import qualified Methods.Admin (Handle, makeH)
import qualified Methods.Author (Handle, makeH)
import qualified Methods.Category (Handle, makeH)
import qualified Methods.Comment (Handle, makeH)
import qualified Methods.Draft (Handle, makeH)
import qualified Methods.Picture (Handle, makeH)
import qualified Methods.Post (Handle, makeH)
import qualified Methods.Tag (Handle, makeH)
import qualified Methods.User (Handle, makeH)

data Handle m = Handle
  { hAdm :: Methods.Admin.Handle m,
    hAu :: Methods.Author.Handle m,
    hCat :: Methods.Category.Handle m,
    hCom :: Methods.Comment.Handle m,
    hDr :: Methods.Draft.Handle m,
    hPic :: Methods.Picture.Handle m,
    hPost :: Methods.Post.Handle m,
    hTag :: Methods.Tag.Handle m,
    hUs :: Methods.User.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let admH = Methods.Admin.makeH conf logH
      auH = Methods.Author.makeH conf logH
      catH = Methods.Category.makeH conf logH
      comH = Methods.Comment.makeH conf logH
      drH = Methods.Draft.makeH conf logH
      picH = Methods.Picture.makeH conf logH
      postH = Methods.Post.makeH conf logH
      tagH = Methods.Tag.makeH conf logH
      userH = Methods.User.makeH conf logH
   in Handle admH auH catH comH drH picH postH tagH userH
