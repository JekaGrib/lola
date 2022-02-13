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
import ParseQueryStr (CreateTag(..),UpdateTag(..),DeleteTag(..))
import Methods.Common (resBuilder)
import           Logger (Priority(..))
import           Data.Text                      ( unpack, Text )
import           Data.ByteString.Builder        ( toLazyByteString )
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Catch            ( throwM)
import Methods.Tag
import Types 


handle :: Handle (StateT (TestDB,[MockAction]) IO)
handle =
  Handle 
    defConf 
    handLogDebug 
    selectTxtsTest 
    updateInDBTest
    deleteFromDbTest 
    isExistInDbTest 
    insertReturnTest 
    id

---withTransactionDBTest :: StateT (TestDB,[MockAction]) IO a -> StateT (TestDB,[MockAction]) IO a
---withTransactionDBTest m = m `catch` (\(e :: SomeException) -> )

insertReturnTest :: String -> String -> [String] -> [Text] -> StateT (TestDB,[MockAction]) IO Integer
insertReturnTest "tags" "tag_id" ["tag_name"] [insValue] = StateT $ \(db,acts) -> 
  return $ insReturnInTags insValue db (INSERTDATA:acts)
insertReturnTest _ _ _ _ = throwM UnexpectedArgsException

insReturnInTags :: Text -> TestDB -> [MockAction] -> (Integer,(TestDB,[MockAction]))
insReturnInTags tN db acts = 
    let tT = tagsT db in
    case tT of
      [] -> (1, ( db {tagsT = [ TagsL 1 tN ]}, acts ))
      _  -> let num = (tag_idTL . last $ tT) + 1 in
        (num, ( db {tagsT = tT ++ [ TagsL num tN ]}, acts ))

selectTxtsTest :: Table -> [Param] -> Where -> [Text] -> StateT (TestDB,[MockAction]) IO [Text]
selectTxtsTest "tags" ["tag_name"] "tag_id=?" [txtNum] = StateT $ \(db,acts) -> 
  return (selectTxtsFromTags txtNum db , (db, SELECTDATA:acts))
selectTxtsTest _ _ _ _ = throwM UnexpectedArgsException

selectTxtsFromTags :: Text -> TestDB -> [Text]
selectTxtsFromTags txtNum db = 
  let tT = tagsT db in
  let numX = read $ unpack txtNum in
    let validLines = filter ( (==) numX . tag_idTL ) tT in
      fmap tag_nameTL validLines

updateInDBTest :: Table -> String -> String -> [Text] -> StateT (TestDB,[MockAction]) IO ()
updateInDBTest "tags" "tag_name=?" "tag_id=?" [tagNameParam,tagIdParam] = StateT $ \(db,acts) -> 
  return $ updateInTags tagNameParam tagIdParam db (UPDATEDATA:acts)
updateInDBTest _ _ _ _ = throwM UnexpectedArgsException

updateInTags :: Text -> Text -> TestDB -> [MockAction] -> ((),(TestDB,[MockAction]))
updateInTags tagNameParam tagIdParam db acts = 
  let numTagId = (read $ unpack tagIdParam :: Integer) in
  let updateFoo line acc = if tag_idTL line == numTagId then  line {tag_nameTL = tagNameParam} : acc else line:acc in
  let newTagsT = foldr updateFoo [] (tagsT db) in
  let newDb = db {tagsT = newTagsT} in
    ((), (newDb,acts))

isExistInDbTest :: Table -> String -> String -> [Text] -> StateT (TestDB,[MockAction]) IO Bool
isExistInDbTest "tags" "tag_id" "tag_id=?" [txtNum] = StateT $ \(db,acts) -> 
  return (isExistInTags txtNum db, (db, EXISTCHEK:acts))
isExistInDbTest _ _ _ _ = throwM UnexpectedArgsException

isExistInTags :: Text -> TestDB -> Bool
isExistInTags txtNum db = (read . unpack $ txtNum) `elem` (fmap tag_idTL . tagsT $ db)

deleteFromDbTest :: Table -> String -> [Text] -> StateT (TestDB,[MockAction]) IO ()
deleteFromDbTest "draftstags" "tag_id=?" [txtNum] = StateT $ \(db,acts) -> 
  return $ deleteFromDraftsTags txtNum db (DELETEDATA:acts)
deleteFromDbTest "poststags" "tag_id=?" [txtNum] = StateT $ \(db,acts) -> 
  return $ deleteFromPostsTags txtNum db (DELETEDATA:acts)
deleteFromDbTest "tags" "tag_id=?" [txtNum] = StateT $ \(db,acts) -> 
  return $ deleteFromTags txtNum db (DELETEDATA:acts)
deleteFromDbTest _ _ _ = throwM UnexpectedArgsException

deleteFromDraftsTags :: Text -> TestDB -> [MockAction] -> ((),(TestDB,[MockAction]))
deleteFromDraftsTags txtNum db acts = 
  let tagId = read . unpack $ txtNum in
  let newDraftsTagsT = filter ((/=) tagId . tag_idDTL) $ draftsTagsT db in
  let newDb = db {draftsTagsT = newDraftsTagsT} in
    ((), (newDb,acts))

deleteFromPostsTags :: Text -> TestDB -> [MockAction] -> ((),(TestDB,[MockAction]))
deleteFromPostsTags txtNum db acts = 
  let tagId = read . unpack $ txtNum in
  let newDraftsTagsT = filter ((/=) tagId . tag_idPTL) $ postsTagsT db in
  let newDb = db {postsTagsT = newDraftsTagsT} in
    ((), (newDb,acts))

deleteFromTags :: Text -> TestDB -> [MockAction] -> ((),(TestDB,[MockAction]))
deleteFromTags txtNum db acts = 
  let tagId = read . unpack $ txtNum in
  let newDraftsTagsT = filter ((/=) tagId . tag_idTL) $ tagsT db in
  let newDb = db {tagsT = newDraftsTagsT} in
    ((), (newDb,acts))

testTag :: IO ()
testTag = hspec $
  describe "Tag" $ do
    it "work" $ do
      state <- execStateT (runExceptT $ createTag handle (CreateTag "cats")) (testDB1,[])
      (reverse . snd $ state) `shouldBe`
        [LOG DEBUG,INSERTDATA,LOG INFO,LOG INFO]
      respE <- evalStateT (runExceptT $ createTag handle (CreateTag "cats")) (testDB1,[])
      let respBuildE = fmap (toLazyByteString . resBuilder) respE
      respBuildE `shouldBe` 
        Right "{\"tag_id\":16,\"tag_name\":\"cats\"}"
    it "work" $ do  
      state <- execStateT (runExceptT $ getTag handle 4) (testDB1,[])
      (reverse . snd $ state) `shouldBe`
        [LOG DEBUG,SELECTDATA,LOG INFO,LOG INFO]
      respE <- evalStateT (runExceptT $ getTag handle 4) (testDB1,[])
      let respBuildE = fmap (toLazyByteString . resBuilder) respE
      respBuildE `shouldBe` 
        Right "{\"tag_id\":4,\"tag_name\":\"Love\"}"
    it "work" $ do  
      state <- execStateT (runExceptT $ updateTag handle (UpdateTag 2 "Salad")) (testDB1,[])
      (reverse . snd $ state) `shouldBe`
        [LOG DEBUG,EXISTCHEK,LOG INFO,LOG DEBUG,UPDATEDATA,LOG INFO,LOG INFO]
      respE <- evalStateT (runExceptT $ updateTag handle (UpdateTag 2 "Salad")) (testDB1,[])
      let respBuildE = fmap (toLazyByteString . resBuilder) respE
      respBuildE `shouldBe` 
        Right "{\"tag_id\":2,\"tag_name\":\"Salad\"}"
    it "work" $ do  
      state <- execStateT (runExceptT $ deleteTag handle (DeleteTag 3)) (testDB1,[])
      (reverse . snd $ state) `shouldBe`
        [LOG DEBUG,EXISTCHEK,LOG INFO,LOG DEBUG,DELETEDATA,DELETEDATA,DELETEDATA,LOG INFO,LOG INFO]
      respE <- evalStateT (runExceptT $ deleteTag handle (DeleteTag 3)) (testDB1,[])
      let respBuildE = fmap (toLazyByteString . resBuilder) respE
      respBuildE `shouldBe` 
        Right "{\"ok\":true}"    

