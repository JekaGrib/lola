{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module TagTest where


import           Test.Hspec (hspec,shouldBe,it,describe)
import           Control.Monad.State (StateT(..),evalStateT,execStateT)           
import TestDB
import TypesTest (MockAction(..))
import ConfTest (defConf)
import OopsTest (UnexpectedArgsException(..))
import LoggerTest (handLogDebug)
import ParseQueryStr (CreateTag(..))
import Methods.Common (resBuilder)
import           Logger (Priority(..))
import           Data.Text                      ( pack, Text )
import           Data.ByteString.Builder        ( toLazyByteString )
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Catch            ( throwM)
import Methods.Tag 


handle :: Handle (StateT (TestDB,[MockAction]) IO)
handle =
  Handle 
    defConf 
    handLogDebug 
    (\_ _ _ _ -> return (fmap pack ["ddd","sss"])) 
    (\_ _ _ _ -> return ()) 
    (\_ _ _ -> return ()) 
    (\_ _ _ _ -> return True) 
    insertReturnInDbTest 
    (id)


insertReturnInDbTest :: String -> String -> [String] -> [Text] -> StateT (TestDB,[MockAction]) IO Integer
insertReturnInDbTest "tags" "tag_id" ["tag_name"] [insValue] = StateT $ \(db,acts) -> do
  return $ insReturnInTags insValue db (INSERTDATA:acts)
insertReturnInDbTest _ _ _ _ = throwM UnexpectedArgsException


insReturnInTags :: Text -> TestDB -> [MockAction] -> (Integer,(TestDB,[MockAction]))
insReturnInTags tN db acts = 
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
        [LOG DEBUG,INSERTDATA,LOG INFO,LOG INFO]
      respE <- evalStateT (runExceptT $ createTag handle (CreateTag "cats")) (testDB1,[])
      let respBuildE = fmap (toLazyByteString . resBuilder) respE
      respBuildE `shouldBe` 
        Right "{\"tag_id\":16,\"tag_name\":\"cats\"}"


