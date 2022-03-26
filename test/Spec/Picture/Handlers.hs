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



handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogWarning
    (selectPicBSTest ["picture"])
    insertRetPicBSTest
    goToUrlTest
    Spec.Auth.Handlers.handle
    Spec.Exist.Handlers.handle

throwSqlEx :: StateT [MockAction] IO a
throwSqlEx = throwM $ SqlError "oops" FatalError "oops" "oops" "oops"

handle1 :: Handle (StateT [MockAction] IO)
handle1 = handle {selectPicBS = selectPicBSTest ["picture", "picture"]}

handle2 :: Handle (StateT [MockAction] IO)
handle2 = handle {selectPicBS = selectPicBSTest []}

handle3 :: Handle (StateT [MockAction] IO)
handle3 =
  handle
    { selectPicBS = selectPicBSTestEx
    }
{-
handle4 :: Handle (StateT [MockAction] IO)
handle4 =
  handle
    { deleteDbTagForDrafts = deleteDbTagForDraftsTestEx
    }
-}
selectPicBSTest :: [ByteString] -> PictureId -> StateT [MockAction] IO [ByteString]  
selectPicBSTest xs picId = do
  modify (PicMock (SelectPicBS picId) :)
  return xs

insertRetPicBSTest :: ByteString -> StateT [MockAction] IO PictureId
insertRetPicBSTest bs = do
  modify (PicMock (InsertReturnPicBS bs) :)
  return 14

goToUrlTest :: Text -> StateT [MockAction] IO BSL.ByteString
goToUrlTest url = do
  modify (PicMock (GoToUrl url) :)
  return "picture"

selectPicBSTestEx :: PictureId -> StateT [MockAction] IO [ByteString]  
selectPicBSTestEx _ = throwSqlEx


insertReturnTagTestEx :: ByteString -> StateT [MockAction] IO PictureId
insertReturnTagTestEx _ = throwSqlEx
