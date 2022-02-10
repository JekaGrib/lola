{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}

module TagTest where


import           Test.Hspec
import           Control.Monad.State            
import TestDB
import TestTypes
import OopsTest
import ParseQueryStr (CreateTag(..))


import           Api.Request
import           Api.Response
import Methods.Common
import Methods.Common.Select 
import           Logger
import Conf 
import Conf.ConnectDB
import           Network.Wai
import           Network.HTTP.Types             ( status200, status404, status301, movedPermanently301, http11 )
import           Network.HTTP.Types.URI         ( queryToQueryText )
import           Network.Wai.Handler.Warp       ( run )
import           Data.Aeson
import           Data.Text                      ( pack, unpack, Text, concat, toUpper, stripPrefix )
import           Data.ByteString.Builder        ( lazyByteString, Builder, toLazyByteString )
import           Database.PostgreSQL.Simple
import qualified Network.HTTP.Simple            as HT
import           Data.Maybe                     ( fromJust )
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Calendar             ( showGregorian, Day, fromGregorian )
import           Database.PostgreSQL.Simple.Time
import           Data.String                    ( fromString )
import           Data.List                      ( intercalate, zip4, nub, find, sortOn, intersect, sort, group, groupBy, isPrefixOf, isInfixOf, isSuffixOf, union )
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
import           Database.PostgreSQL.Simple.ToRow
import           Prelude                        as P
import Methods.Tag 


emptyConnectInfo = ConnectInfo "" 0 "" "" ""

emptyConnDB = ConnDB _ emptyConnDBInfo

defConf = Config "localhost" 3000 emptyConnDB DEBUG 1 1 1 1 20 5 5 


handle :: Handle (StateT (TestDB,[MockAction]) IO)
handle =
  Handle 
    defConf 
    handleLog1 
    (\_ _ _ _ -> return (fmap pack ["ddd","sss"])) 
    (\_ _ _ _ -> return ()) 
    (\_ _ _ -> return ()) 
    (\_ _ _ _ -> return True) 
    insertReturnInDbTest 
    (id)


handleLog1 = LogHandle (LogConfig DEBUG) logTest

logTest :: Priority -> String -> StateT (TestDB,[MockAction]) IO ()
logTest prio text = StateT $ \(db,acts) -> 
  return (() , (db,LOG prio : acts))


insertReturnInDbTest :: String -> String -> [String] -> [Text] -> StateT (TestDB,[MockAction]) IO Integer
insertReturnInDbTest table returnName insNames insValues = StateT $ \(db,acts) -> do
  return $ insReturn table returnName insNames insValues db (INSERTDATA:acts)


insReturn ::  String -> String -> [String] -> [Text] -> TestDB -> [MockAction] -> (Integer,(TestDB,[MockAction]))
insReturn "tags" returnName insNames insValues db acts = insReturnInTags returnName insNames insValues db acts

insReturnInTags :: String -> [String] -> [Text] -> TestDB -> [MockAction] -> (Integer,(TestDB,[MockAction]))
insReturnInTags "tag_id" ["tag_name"] [tN] db acts = 
    let tT = tagsT db in
    case tT of
      [] -> (1, ( db {tagsT = [ TagsL 1 tN ]}, acts ))
      _  -> let num = (tag_idTL . last $ tT) + 1 in
        (num, ( db {tagsT = tT ++ [ TagsL num tN ]}, acts ))

testTag :: IO ()
testTag = hspec $ do
  describe "Tag" $ do
    it "work" $ do
      state <- execStateT (runExceptT $ createTag handle (CreateTag "cats")) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOG INFO,EXISTCHEK,LOG DEBUG]
      respE <- evalStateT (runExceptT $ createTag handle (CreateTag "cats")) (testDB1,[])
      let respBuildE = fmap (toLazyByteString . resBuilder) respE
      respBuildE `shouldBe` 
        Right "{\"ok\":true}"


