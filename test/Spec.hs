{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}


import           Test.Hspec
import           Control.Monad.State            


import           App
import           Api
import           Logger
import           Network.Wai
import           Network.HTTP.Types             ( status200, status404, status301, movedPermanently301, http11 )
import           Network.HTTP.Types.URI         ( queryToQueryText )
import           Network.Wai.Handler.Warp       ( run )
import           Data.Aeson
import           Data.Text                      ( pack, unpack, Text, concat, toUpper, stripPrefix, isPrefixOf )
import           Data.ByteString.Builder        ( lazyByteString )
import           Database.PostgreSQL.Simple
import qualified Network.HTTP.Simple            as HT
import           Data.Maybe                     ( fromJust )
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Calendar             ( showGregorian, Day )
import           Database.PostgreSQL.Simple.Time
import           Data.String                    ( fromString )
import           Data.List                      ( intercalate, zip4 )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )
import           Codec.Picture                  ( decodeImage )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy           as BSL
import           Control.Monad.Catch            ( catch, throwM, MonadCatch )
import           Data.HashMap.Strict            ( toList, fromList )
import qualified Data.Vector                    as V
import           Data.Int                       ( Int64 )
import qualified Database.PostgreSQL.Simple.FromField as FF
import qualified Data.Map                       as M

data MockAction = EXISTCHEK | LOGMSG
  deriving (Eq,Show)
type TestDB = M.Map Integer Text 

logTest :: Priority -> String -> StateT (TestDB,[MockAction]) IO ()
logTest prio text = StateT $ \(db,acts) -> 
  return (() , (db,LOGMSG : acts))


testDB1 = M.fromList [(1,"fill"),(2,"call"),(3,"bill")]

isExistInDbTest :: String -> String -> String -> [Text] -> StateT (TestDB,[MockAction]) IO Bool
isExistInDbTest s s' s'' (x:xs) = StateT $ \(db,acts) -> do
  return ( M.member (read . unpack $ x) db , (db,EXISTCHEK:acts))


handleLog1 = LogHandle (LogConfig DEBUG) logTest
handle1 = Handle 
  { hLog = handleLog1,
    isExistInDb = isExistInDbTest
    }

reqTest = defaultRequest {requestMethod = "GET", httpVersion = http11, rawPathInfo = "/test/3", rawQueryString = "", requestHeaders = [("User-Agent","PostmanRuntime/7.26.8"),("Accept","*/*"),("Postman-Token","6189d61d-fa65-4fb6-a578-c4061535e7ef"),("Host","localhost:3000"),("Accept-Encoding","gzip, deflate, br"),("Connection","keep-alive"),("Content-Type","multipart/form-data; boundary=--------------------------595887703656508108682668"),("Content-Length","170")], isSecure = False, pathInfo = ["test","3"], queryString = [], requestBodyLength = KnownLength 170, requestHeaderHost = Just "localhost:3000", requestHeaderRange = Nothing}

main :: IO ()
main = hspec $ do
  describe "CheckExist" $ do
    it "work" $ do
      let conn = undefined
      state <- execStateT (runExceptT $ answerEx handle1 conn reqTest) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG]

