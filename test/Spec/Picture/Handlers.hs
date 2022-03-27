{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Picture.Handlers where

import Control.Monad.Catch (throwM)
import Control.Monad.State (StateT (..), modify)
import Database.PostgreSQL.Simple (ExecStatus (FatalError), SqlError (..))
import Methods.Picture
import qualified Spec.Auth.Handlers (handle)
import Spec.Conf (defConf)
import qualified Spec.Exist.Handlers (handle)
import Spec.Log (handLogWarning)
import Spec.Picture.Types
import Spec.Types (MockAction (..))
import Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import Codec.Picture (encodePng,PixelRGB8(..),generateImage)
import Network.HTTP.Simple (HttpException( InvalidUrlException ))



handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogWarning
    (selectPicBSTest ["picture"])
    insertRetPicBSTest
    (goToUrlTest bsPicExample)
    Spec.Auth.Handlers.handle
    Spec.Exist.Handlers.handle

throwSqlEx :: StateT [MockAction] IO a
throwSqlEx = throwM $ SqlError "oops" FatalError "oops" "oops" "oops"

throwHttpEx :: StateT [MockAction] IO a
throwHttpEx = throwM $ InvalidUrlException "oops" "oops"

handle1 :: Handle (StateT [MockAction] IO)
handle1 = handle {selectPicBS = selectPicBSTest ["picture", "picture"]}

handle2 :: Handle (StateT [MockAction] IO)
handle2 = handle {selectPicBS = selectPicBSTest []}

handle3 :: Handle (StateT [MockAction] IO)
handle3 =
  handle
    { selectPicBS = selectPicBSTestEx
    , goToUrl = goToUrlTestEx
    }

handle4 :: Handle (StateT [MockAction] IO)
handle4 =
  handle
    { goToUrl = goToUrlTest "oops"
    }

handle5 :: Handle (StateT [MockAction] IO)
handle5 =
  handle
    { insertRetPicBS = insertRetPicBSTestEx
    }

selectPicBSTest :: [ByteString] -> PictureId -> StateT [MockAction] IO [ByteString]  
selectPicBSTest xs picId = do
  modify (PicMock (SelectPicBS picId) :)
  return xs

insertRetPicBSTest :: ByteString -> StateT [MockAction] IO PictureId
insertRetPicBSTest bs = do
  modify (PicMock (InsertReturnPicBS bs) :)
  return 14

goToUrlTest :: BSL.ByteString -> Text -> StateT [MockAction] IO BSL.ByteString
goToUrlTest bs url = do
  modify (PicMock (GoToUrl url) :)
  return bs

selectPicBSTestEx :: PictureId -> StateT [MockAction] IO [ByteString]  
selectPicBSTestEx _ = throwSqlEx


insertRetPicBSTestEx :: ByteString -> StateT [MockAction] IO PictureId
insertRetPicBSTestEx _ = throwSqlEx

goToUrlTestEx :: Text -> StateT [MockAction] IO BSL.ByteString
goToUrlTestEx _ = throwHttpEx

bsPicExample :: BSL.ByteString
bsPicExample = encodePng $ generateImage pixelRenderer 1 1
   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 1

sbsPicExample :: ByteString
sbsPicExample = BSL.toStrict bsPicExample