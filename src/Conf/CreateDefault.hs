{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Conf.CreateDefault (createNewDefPic, createNewDefUser, createNewDefAuthor, createNewDefCat) where

import Codec.Picture (decodeImage)
import Control.Monad.Catch (catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.String (fromString)
import Data.Text (pack)
import Data.Time.Calendar (showGregorian)
import Data.Time.LocalTime (getZonedTime, localDay, zonedTimeToLocalTime)
import Database.PostgreSQL.Simple (Binary (Binary), Connection, Only (Only), query, query_)
import qualified Network.HTTP.Simple as HT
import Types

createNewDefPic :: Connection -> IO PictureId
createNewDefPic conn = do
  defPicBs <- enterUrlAndGetPicBs
  picId <- createDefaultPicture conn defPicBs
  putStrLn $ "Default picture created, id:" ++ show picId
  return picId

enterUrlAndGetPicBs :: IO ByteString
enterUrlAndGetPicBs = do
  putStrLn "Enter default picture url"
  input <- getLine
  ( do
      res <- HT.httpLBS . fromString $ input
      let lbs = HT.getResponseBody res
      let sbs = BSL.toStrict lbs
      case decodeImage sbs of
        Right _ -> return sbs
        Left _ -> do
          putStrLn $ "Invalid picture url:" ++ input
          enterUrlAndGetPicBs
    )
    `catch` ( \e -> do
                putStrLn $ "Invalid picture url:" ++ input ++ ". " ++ show (e :: HT.HttpException)
                enterUrlAndGetPicBs
            )

createDefaultPicture :: Connection -> ByteString -> IO PictureId
createDefaultPicture conn defPicBs = do
  [Only picId] <- query conn "INSERT INTO pics ( pic ) VALUES (?) RETURNING pic_id " [Binary defPicBs]
  return picId

createNewDefUser :: Connection -> PictureId -> IO UserId
createNewDefUser conn picId = do
  userId <- createDefaultUser conn picId
  putStrLn $ "Default user created, id:" ++ show userId
  return userId

createDefaultUser :: Connection -> PictureId -> IO UserId
createDefaultUser conn picId = do
  day <- getDay
  [Only userId] <- query conn "INSERT INTO users ( password, first_name , last_name , user_pic_id , user_create_date, admin,token_key) VALUES ( '12345678','DELETED','DELETED',?,?, false, 'lolalola' ) RETURNING user_id" [pack (show picId), pack day]
  return userId

getDay :: IO String
getDay = do
  time <- getZonedTime
  let day = showGregorian . localDay . zonedTimeToLocalTime $ time
  return day

createNewDefAuthor :: Connection -> UserId -> IO AuthorId
createNewDefAuthor conn userId = do
  authorId <- createDefaultAuthor conn userId
  putStrLn $ "Default author created, id:" ++ show authorId
  return authorId

createDefaultAuthor :: Connection -> UserId -> IO AuthorId
createDefaultAuthor conn userId = do
  [Only authorId] <- query conn "INSERT INTO authors ( user_id , author_info) VALUES ( ?,'DELETED' ) RETURNING author_id" [pack . show $ userId]
  return authorId

createNewDefCat :: Connection -> IO CategoryId
createNewDefCat conn = do
  catId <- createDefaultCategory conn
  putStrLn $ "Default category created, id:" ++ show catId
  return catId

createDefaultCategory :: Connection -> IO CategoryId
createDefaultCategory conn = do
  [Only catId] <- query_ conn "INSERT INTO categories (category_name) VALUES ( 'NONE' ) RETURNING category_id"
  return catId

{-addCreateAdminKey :: Connection -> IO ()
addCreateAdminKey conn = do
  _ <- execute_ conn "INSERT INTO key (create_admin_key) VALUES ( 'lola' ) "
  return ()-}
