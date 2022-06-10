{-# LANGUAGE FlexibleInstances #-}

module Methods.Common.Exist where

import Conf (Config (..), extractConn)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Methods.Common
import Methods.Common.Exist.UncheckedExId
import Error (ReqError (..))
import Psql.Methods.Common.Exist

data Handle m = Handle
  { hConf :: Config,
    isExist :: UncheckedExId -> m Bool
  }

makeH :: Config -> Handle IO
makeH conf =
  let conn = extractConn conf
   in Handle
        conf
        (isExist' conn)

isExistResourseE :: (MonadCatch m) => Handle m -> UncheckedExId -> ExceptT ReqError m ()
isExistResourseE h iD = do
  isEx <- catchDbErrE $ isExist h iD
  unless isEx
    $ throwE
    $ ResourseNotExistError
    $ toPretty iD ++ " doesn`t exist"

isExistE :: (MonadCatch m) => Handle m -> UncheckedExId -> ExceptT ReqError m ()
isExistE h iD = do
  isEx <- catchDbErrE $ isExist h iD
  unless isEx
    $ throwE
    $ BadReqError
    $ toPretty iD ++ " doesn`t exist"

class CheckExist a where
  checkExist :: (MonadCatch m) => Handle m -> a -> ExceptT ReqError m ()

instance CheckExist UncheckedExId where
  checkExist = isExistE

instance CheckExist [UncheckedExId] where
  checkExist h = mapM_ (isExistE h)

instance CheckExist a => CheckExist (Maybe a) where
  checkExist _ Nothing = return ()
  checkExist h (Just iD) = checkExist h iD


