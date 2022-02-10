{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}


import           Test.Hspec
import           Control.Monad.State            


import           App
import           Api.Request
import           Api.Response
import Methods.Handle
import Methods.Post.LimitArg
import Methods.Handle.Select 
import           Logger
import Conf 
import ConnectDB
import SelectTest
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


main :: IO ()
main = hspec $ do
  describe "CheckExist" $ do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest0) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest0) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe` 
        "{\"ok\":true}"
  describe "createUser" $ do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest1) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,INSERTDATA,LOGMSG,INSERTDATA,LOGMSG,LOGMSG,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest1) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe` 
        "{\"user_id\":11,\"first_name\":\"Kate\",\"last_name\":\"Grick\",\"user_pic_id\":11,\"user_pic_url\":\"http://localhost:3000/picture/11\",\"user_create_date\":\"2020-02-20\"}"
  describe "getUser" $ do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest2) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]      
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest2) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe` 
        "{\"user_id\":3,\"first_name\":\"Ira\",\"last_name\":\"Medvedeva\",\"user_pic_id\":2,\"user_pic_url\":\"http://localhost:3000/picture/2\",\"user_create_date\":\"2018-03-01\"}"
  describe "deleteUser" $ do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest3) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,UPDATEDATA,LOGMSG,EXISTCHEK,DELETEDATA,LOGMSG]      
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest3) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "deleteUser AuthorTest" $ do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest4) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,UPDATEDATA,LOGMSG,EXISTCHEK,LOGMSG,SELECTDATA,LOGMSG,UPDATEDATA,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA,DELETEDATA,LOGMSG]      
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest4) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "createAdmin" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest5) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTDATA,LOGMSG,LOGMSG,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest5) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"user_id\":11,\"first_name\":\"Chris\",\"last_name\":\"Wirt\",\"user_pic_id\":11,\"user_pic_url\":\"http://localhost:3000/picture/11\",\"user_create_date\":\"2020-02-20\"}"
  describe "createAuthor" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest6) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,INSERTDATA,LOGMSG,LOGMSG,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest6) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"author_id\":6,\"author_info\":\"SuperAuthor\",\"user_id\":3}"    
  describe "getAuthor" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest7) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest7) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"author_id\":3,\"author_info\":\"London is the capital\",\"user_id\":6}"
  describe "updateAuthor" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest8) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,EXISTCHEK,UPDATEDATA]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest8) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"author_id\":3,\"author_info\":\"Very funny\",\"user_id\":7}"
  describe "deleteAuthor" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest9) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,UPDATEDATA,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest9) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "createCategory without admin params" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest10) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest10) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "Status 404 Not Found"
  describe "createCategory" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest11) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,INSERTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest11) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"category_id\":17,\"category_name\":\"Juzz\",\"sub_categories\":[],\"super_category\":\"NULL\"}"
  describe "createSubCategory" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest12) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,INSERTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest12) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"category_id\":17,\"category_name\":\"France\",\"sub_categories\":[],\"super_category\":{\"category_id\":12,\"category_name\":\"Europe\",\"sub_categories\":[13,15,17],\"super_category\":{\"category_id\":11,\"category_name\":\"Place\",\"sub_categories\":[12,14],\"super_category\":\"NULL\"}}}"
  describe "getCategory" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest13) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest13) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"category_id\":14,\"category_name\":\"Africa\",\"sub_categories\":[16],\"super_category\":{\"category_id\":11,\"category_name\":\"Place\",\"sub_categories\":[12,14],\"super_category\":\"NULL\"}}"
  describe "updateCategory" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest14) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,UPDATEDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest14) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"category_id\":5,\"category_name\":\"Games\",\"sub_categories\":[9,10],\"super_category\":{\"category_id\":6,\"category_name\":\"Hobby\",\"sub_categories\":[5,7,8],\"super_category\":\"NULL\"}}"
  describe "deleteCategory" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest15) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,UPDATEDATA,UPDATEDATA,DELETEDATA]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest15) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "createTag" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest16) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,INSERTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest16) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"tag_id\":16,\"tag_name\":\"Bears\"}"
  describe "getTag" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest17) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest17) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"tag_id\":12,\"tag_name\":\"Medicine\"}"
  describe "updateTag" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest18) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,UPDATEDATA]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest18) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"tag_id\":7,\"tag_name\":\"King\"}"
  describe "deleteTag" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest19) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA,DELETEDATA,DELETEDATA]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest19) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "createNewDraft" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest20) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTMANYDATA,INSERTMANYDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest20) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"draft_id\":6,\"post_id\":\"NULL\",\"author\":{\"author_id\":3,\"author_info\":\"London is the capital\",\"user_id\":6},\"draft_name\":\"rock\",\"draft_category\":{\"category_id\":3,\"category_name\":\"football\",\"sub_categories\":[],\"super_category\":{\"category_id\":2,\"category_name\":\"Sport\",\"sub_categories\":[3,4,5],\"super_category\":\"NULL\"}},\"draft_text\":\"heyhey\",\"draft_main_pic_id\":11,\"draft_main_pic_url\":\"http://localhost:3000/picture/11\",\"draft_pics\":[{\"pic_id\":12,\"pic_url\":\"http://localhost:3000/picture/12\"},{\"pic_id\":13,\"pic_url\":\"http://localhost:3000/picture/13\"}],\"draft_tags\":[]}"  
  describe "createPostsDraft" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest21) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,INSERTDATA,LOGMSG]      
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest21) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"draft_id\":6,\"post_id\":3,\"author\":{\"author_id\":4,\"author_info\":\"i have a cat\",\"user_id\":8},\"draft_name\":\"Sorry\",\"draft_category\":{\"category_id\":2,\"category_name\":\"Sport\",\"sub_categories\":[3,4,5],\"super_category\":\"NULL\"},\"draft_text\":\"some consumers argue that advertising is a bad thing. They say that advertising is bad for children. Adverts make children \226\128\152pester\226\128\153 their parents to buy things for them. Advertisers know we love our children and want to give them everything.\",\"draft_main_pic_id\":3,\"draft_main_pic_url\":\"http://localhost:3000/picture/3\",\"draft_pics\":[{\"pic_id\":7,\"pic_url\":\"http://localhost:3000/picture/7\"},{\"pic_id\":8,\"pic_url\":\"http://localhost:3000/picture/8\"}],\"draft_tags\":[{\"tag_id\":7,\"tag_name\":\"Autumn\"}]}"
  describe "createPostsDraft. UserTest not author" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest22) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest22) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":false,\"info\":\"user_id: 4 is not author of post_id: 3\"}"
  describe "getDraft" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest23) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest23) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"draft_id\":4,\"post_id\":3,\"author\":{\"author_id\":4,\"author_info\":\"i have a cat\",\"user_id\":8},\"draft_name\":\"Table\",\"draft_category\":{\"category_id\":14,\"category_name\":\"Africa\",\"sub_categories\":[16],\"super_category\":{\"category_id\":11,\"category_name\":\"Place\",\"sub_categories\":[12,14],\"super_category\":\"NULL\"}},\"draft_text\":\"People say that travelling is dangerous, for example, driving a car. They point to the fact that there are so many cars on the roads that the chances of an accident are very high. But that\226\128\153s nothing compared to Space. Space will soon be so dangerous to travel in that only a mad man would even try.\",\"draft_main_pic_id\":2,\"draft_main_pic_url\":\"http://localhost:3000/picture/2\",\"draft_pics\":[{\"pic_id\":6,\"pic_url\":\"http://localhost:3000/picture/6\"},{\"pic_id\":8,\"pic_url\":\"http://localhost:3000/picture/8\"},{\"pic_id\":9,\"pic_url\":\"http://localhost:3000/picture/9\"},{\"pic_id\":10,\"pic_url\":\"http://localhost:3000/picture/10\"},{\"pic_id\":3,\"pic_url\":\"http://localhost:3000/picture/3\"}],\"draft_tags\":[{\"tag_id\":8,\"tag_name\":\"Spring\"}]}"
  describe "getDrafts page1" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest24) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTLIMITDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest24) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"page\":1,\"drafts\":[{\"draft_id\":5,\"post_id\":\"NULL\",\"author\":{\"author_id\":4,\"author_info\":\"i have a cat\",\"user_id\":8},\"draft_name\":\"Cort\",\"draft_category\":{\"category_id\":3,\"category_name\":\"football\",\"sub_categories\":[],\"super_category\":{\"category_id\":2,\"category_name\":\"Sport\",\"sub_categories\":[3,4,5],\"super_category\":\"NULL\"}},\"draft_text\":\"The reason is simple; ever since we started exploring Space in the late 1950s we have been leaving things up there. There is now so much rubbish circling the Earth that from a distance our planet appears to have a ring around it, making it look a bit like Saturn. Unless we start cleaning up after ourselves, we are in danger.\",\"draft_main_pic_id\":3,\"draft_main_pic_url\":\"http://localhost:3000/picture/3\",\"draft_pics\":[{\"pic_id\":7,\"pic_url\":\"http://localhost:3000/picture/7\"},{\"pic_id\":6,\"pic_url\":\"http://localhost:3000/picture/6\"}],\"draft_tags\":[{\"tag_id\":4,\"tag_name\":\"Love\"},{\"tag_id\":6,\"tag_name\":\"Sommer\"},{\"tag_id\":9,\"tag_name\":\"Mondey\"}]},{\"draft_id\":4,\"post_id\":3,\"author\":{\"author_id\":4,\"author_info\":\"i have a cat\",\"user_id\":8},\"draft_name\":\"Table\",\"draft_category\":{\"category_id\":14,\"category_name\":\"Africa\",\"sub_categories\":[16],\"super_category\":{\"category_id\":11,\"category_name\":\"Place\",\"sub_categories\":[12,14],\"super_category\":\"NULL\"}},\"draft_text\":\"People say that travelling is dangerous, for example, driving a car. They point to the fact that there are so many cars on the roads that the chances of an accident are very high. But that\226\128\153s nothing compared to Space. Space will soon be so dangerous to travel in that only a mad man would even try.\",\"draft_main_pic_id\":2,\"draft_main_pic_url\":\"http://localhost:3000/picture/2\",\"draft_pics\":[{\"pic_id\":6,\"pic_url\":\"http://localhost:3000/picture/6\"},{\"pic_id\":8,\"pic_url\":\"http://localhost:3000/picture/8\"},{\"pic_id\":9,\"pic_url\":\"http://localhost:3000/picture/9\"},{\"pic_id\":10,\"pic_url\":\"http://localhost:3000/picture/10\"},{\"pic_id\":3,\"pic_url\":\"http://localhost:3000/picture/3\"}],\"draft_tags\":[{\"tag_id\":8,\"tag_name\":\"Spring\"}]}]}"
  describe "getDrafts page2" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest25) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTLIMITDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest25) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"page\":2,\"drafts\":[]}"
  describe "updateDraft" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest26) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTDATA,LOGMSG,DELETEDATA,DELETEDATA,UPDATEDATA,INSERTMANYDATA,INSERTMANYDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest26) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"draft_id\":5,\"post_id\":\"NULL\",\"author\":{\"author_id\":3,\"author_info\":\"London is the capital\",\"user_id\":6},\"draft_name\":\"rock\",\"draft_category\":{\"category_id\":3,\"category_name\":\"football\",\"sub_categories\":[],\"super_category\":{\"category_id\":2,\"category_name\":\"Sport\",\"sub_categories\":[3,4,5],\"super_category\":\"NULL\"}},\"draft_text\":\"heyhey\",\"draft_main_pic_id\":11,\"draft_main_pic_url\":\"http://localhost:3000/picture/11\",\"draft_pics\":[{\"pic_id\":12,\"pic_url\":\"http://localhost:3000/picture/12\"},{\"pic_id\":13,\"pic_url\":\"http://localhost:3000/picture/13\"}],\"draft_tags\":[]}"
  describe "deleteDraft" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest27) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA,DELETEDATA,DELETEDATA]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest27) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"  
  describe "publishDraft" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest28) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTMANYDATA,INSERTMANYDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest28) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"post_id\":6,\"author\":{\"author_id\":4,\"author_info\":\"i have a cat\",\"user_id\":8},\"post_name\":\"Cort\",\"post_create_date\":\"2020-02-20\",\"post_category\":{\"category_id\":3,\"category_name\":\"football\",\"sub_categories\":[],\"super_category\":{\"category_id\":2,\"category_name\":\"Sport\",\"sub_categories\":[3,4,5],\"super_category\":\"NULL\"}},\"post_text\":\"The reason is simple; ever since we started exploring Space in the late 1950s we have been leaving things up there. There is now so much rubbish circling the Earth that from a distance our planet appears to have a ring around it, making it look a bit like Saturn. Unless we start cleaning up after ourselves, we are in danger.\",\"post_main_pic_id\":3,\"post_main_pic_url\":\"http://localhost:3000/picture/3\",\"post_pics\":[{\"pic_id\":7,\"pic_url\":\"http://localhost:3000/picture/7\"},{\"pic_id\":6,\"pic_url\":\"http://localhost:3000/picture/6\"}],\"post_tags\":[{\"tag_id\":4,\"tag_name\":\"Love\"},{\"tag_id\":6,\"tag_name\":\"Sommer\"},{\"tag_id\":9,\"tag_name\":\"Mondey\"}]}"
  describe "publishDraft  (posts draft)" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest29) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,UPDATEDATA,DELETEDATA,DELETEDATA,INSERTMANYDATA,INSERTMANYDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest29) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"post_id\":3,\"author\":{\"author_id\":4,\"author_info\":\"i have a cat\",\"user_id\":8},\"post_name\":\"Table\",\"post_create_date\":\"2019-07-25\",\"post_category\":{\"category_id\":14,\"category_name\":\"Africa\",\"sub_categories\":[16],\"super_category\":{\"category_id\":11,\"category_name\":\"Place\",\"sub_categories\":[12,14],\"super_category\":\"NULL\"}},\"post_text\":\"People say that travelling is dangerous, for example, driving a car. They point to the fact that there are so many cars on the roads that the chances of an accident are very high. But that\226\128\153s nothing compared to Space. Space will soon be so dangerous to travel in that only a mad man would even try.\",\"post_main_pic_id\":2,\"post_main_pic_url\":\"http://localhost:3000/picture/2\",\"post_pics\":[{\"pic_id\":6,\"pic_url\":\"http://localhost:3000/picture/6\"},{\"pic_id\":8,\"pic_url\":\"http://localhost:3000/picture/8\"},{\"pic_id\":9,\"pic_url\":\"http://localhost:3000/picture/9\"},{\"pic_id\":10,\"pic_url\":\"http://localhost:3000/picture/10\"},{\"pic_id\":3,\"pic_url\":\"http://localhost:3000/picture/3\"}],\"post_tags\":[{\"tag_id\":8,\"tag_name\":\"Spring\"}]}"
  describe "getPost" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest30) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest30) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"post_id\":2,\"author\":{\"author_id\":2,\"author_info\":\"i don`t like it\",\"user_id\":4},\"post_name\":\"Glass\",\"post_create_date\":\"2019-06-01\",\"post_category\":{\"category_id\":16,\"category_name\":\"Egypt\",\"sub_categories\":[],\"super_category\":{\"category_id\":14,\"category_name\":\"Africa\",\"sub_categories\":[16],\"super_category\":{\"category_id\":11,\"category_name\":\"Place\",\"sub_categories\":[12,14],\"super_category\":\"NULL\"}}},\"post_text\":\"Advertising companies say advertising is necessary and important. It informs people about new products. Advertising hoardings in the street make our environment colourful. And adverts on TV are often funny. Sometimes they are mini-dramas and we wait for the next programme in the mini-drama. Advertising can educate, too. Adverts tell us about new, healthy products\",\"post_main_pic_id\":2,\"post_main_pic_url\":\"http://localhost:3000/picture/2\",\"post_pics\":[],\"post_tags\":[{\"tag_id\":1,\"tag_name\":\"Cats\"},{\"tag_id\":12,\"tag_name\":\"Medicine\"},{\"tag_id\":10,\"tag_name\":\"Home\"}]}"
  describe "deletePost" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest31) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA,DELETEDATA,DELETEDATA,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest31) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "createComment" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest32) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,INSERTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest32) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"comment_id\":11,\"comment_text\":\"sweet\",\"post_id\":3,\"user_id\":8}"
  describe "updateComment" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest33) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,UPDATEDATA,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest33) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"comment_id\":4,\"comment_text\":\"creepy\",\"post_id\":2,\"user_id\":2}"
  describe "deleteComment (admin) " $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest34) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest34) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "deleteComment (comment owner) " $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest35) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest35) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "getPosts" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ chooseRespEx handle1 reqTest36) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,SELECTLIMITDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ chooseRespEx handle1 reqTest36) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"page\":1,\"posts\":[{\"post_id\":2,\"author\":{\"author_id\":2,\"author_info\":\"i don`t like it\",\"user_id\":4},\"post_name\":\"Glass\",\"post_create_date\":\"2019-06-01\",\"post_category\":{\"category_id\":16,\"category_name\":\"Egypt\",\"sub_categories\":[],\"super_category\":{\"category_id\":14,\"category_name\":\"Africa\",\"sub_categories\":[16],\"super_category\":{\"category_id\":11,\"category_name\":\"Place\",\"sub_categories\":[12,14],\"super_category\":\"NULL\"}}},\"post_text\":\"Advertising companies say advertising is necessary and important. It informs people about new products. Advertising hoardings in the street make our environment colourful. And adverts on TV are often funny. Sometimes they are mini-dramas and we wait for the next programme in the mini-drama. Advertising can educate, too. Adverts tell us about new, healthy products\",\"post_main_pic_id\":2,\"post_main_pic_url\":\"http://localhost:3000/picture/2\",\"post_pics\":[],\"post_tags\":[{\"tag_id\":1,\"tag_name\":\"Cats\"},{\"tag_id\":12,\"tag_name\":\"Medicine\"},{\"tag_id\":10,\"tag_name\":\"Home\"}]}]}"


