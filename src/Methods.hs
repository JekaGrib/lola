--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}




module Methods where
          
import           Logger (LogHandle)
import  Conf (Config)
import qualified Methods.Admin (makeH,Handle)
import qualified Methods.Auth (makeH,Handle)
import qualified Methods.Author (makeH,Handle)
import qualified Methods.Category (makeH,Handle)
import qualified Methods.Comment (makeH,Handle)
import qualified Methods.Draft (makeH,Handle)
import qualified Methods.Picture (makeH,Handle)
import qualified Methods.Post (makeH,Handle)
import qualified Methods.Tag (makeH,Handle)
import qualified Methods.User (makeH,Handle)

data Handle m = Handle 
  { hAdm          :: Methods.Admin.Handle m,
    hAuth         :: Methods.Auth.Handle m,
    hAu          :: Methods.Author.Handle m,
    hCat          :: Methods.Category.Handle m,
    hCom          :: Methods.Comment.Handle m,
    hDr          :: Methods.Draft.Handle m,
    hPic          :: Methods.Picture.Handle m,
    hPost          :: Methods.Post.Handle m,
    hTag          :: Methods.Tag.Handle m,
    hUs          :: Methods.User.Handle m
    }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH = 
  let admH = Methods.Admin.makeH conf logH 
      authH = Methods.Auth.makeH conf logH 
      auH = Methods.Author.makeH conf logH 
      catH = Methods.Category.makeH conf logH 
      comH = Methods.Comment.makeH conf logH 
      drH = Methods.Draft.makeH conf logH 
      picH = Methods.Picture.makeH conf logH 
      postH = Methods.Post.makeH conf logH 
      tagH = Methods.Tag.makeH conf logH 
      userH = Methods.User.makeH conf logH
  in Handle admH authH auH catH comH drH picH postH tagH userH





