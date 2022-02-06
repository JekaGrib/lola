--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module Conf.CreateDefault (createNewDefPic, createNewDefUser, createNewDefAuthor, createNewDefCat) where


import ConnectDB (ConnDB)
import Types
import           Logger
import ConnectDB (tryConnect,ConnDB(..),inputString,inputInteger)
import           Data.Text                      ( Text, pack, unpack, intercalate )
import           Network.Wai.Handler.Warp       ( run )
import           Database.PostgreSQL.Simple
import qualified Network.HTTP.Simple            as HT
import           Data.Time.LocalTime
import           Data.String                    ( fromString )
import           Control.Monad.Catch            ( catch )
import qualified Data.Configurator              as C
import qualified Data.Configurator.Types        as C
import qualified Control.Exception              as E
import qualified Data.ByteString.Lazy           as BSL
import           Codec.Picture                  ( decodeImage )
import           Data.Char                      ( toUpper )
import           Data.ByteString                ( ByteString )
import Methods.Handle.ToQuery (toExQ)
import           Data.Time.Calendar             ( showGregorian)



createNewDefPic :: Connection -> IO Integer
createNewDefPic conn = do
  defPicBs <- enterUrlAndGetPicBs
  picId <- createDefaultPicture conn defPicBs
  putStrLn $ "Default picture created, id:" ++ show picId
  return picId

enterUrlAndGetPicBs :: IO ByteString
enterUrlAndGetPicBs = do
  putStrLn $ "Enter default picture url"
  input <- getLine
  (do
    res <- HT.httpLBS . fromString $ input  
    let lbs = HT.getResponseBody res
    let sbs = BSL.toStrict lbs
    case decodeImage sbs of
      Right _ -> return sbs
      Left _  -> do
        putStrLn $ "Invalid picture url:" ++ input
        enterUrlAndGetPicBs) `catch` (\e -> do
          putStrLn $ "Invalid picture url:" ++ input ++ ". " ++ (show (e :: HT.HttpException))
          enterUrlAndGetPicBs)

createDefaultPicture :: Connection -> ByteString -> IO Integer
createDefaultPicture conn defPicBs = do
  [Only picId] <- query conn "INSERT INTO pics ( pic ) VALUES (?) RETURNING pic_id " [Binary defPicBs]
  return picId

createNewDefUser :: Connection -> Integer -> IO Integer
createNewDefUser conn picId = do
  userId <- createDefaultUser conn picId
  putStrLn $ "Default user created, id:" ++ show userId
  return userId

createDefaultUser :: Connection -> Integer -> IO Integer
createDefaultUser conn picId = do
  day <- getDay  
  [Only userId] <- query conn "INSERT INTO users ( password, first_name , last_name , user_pic_id , user_create_date, admin) VALUES ( '12345678','DELETED','DELETED',?,?, false ) RETURNING user_id" [ pack (show picId), pack day ]
  return userId

getDay :: IO String
getDay = do
  time    <- getZonedTime
  let day = showGregorian . localDay . zonedTimeToLocalTime $ time
  return day

createNewDefAuthor :: Connection -> Integer -> IO Integer
createNewDefAuthor conn userId = do
  authorId <- createDefaultAuthor conn userId
  putStrLn $ "Default author created, id:" ++ show authorId
  return authorId

createDefaultAuthor :: Connection -> Integer -> IO Integer
createDefaultAuthor conn userId = do
  [Only authorId] <- query conn "INSERT INTO authors ( user_id , author_info) VALUES ( ?,'DELETED' ) RETURNING author_id" [pack . show $ userId]  
  return authorId

createNewDefCat :: Connection -> IO Integer
createNewDefCat conn = do
  catId <- createDefaultCategory conn
  putStrLn $ "Default category created, id:" ++ show catId
  return catId

createDefaultCategory :: Connection -> IO Integer
createDefaultCategory conn = do
  [Only catId] <- query_ conn "INSERT INTO categories (category_name) VALUES ( 'NONE' ) RETURNING category_id" 
  return catId


{-addCreateAdminKey :: Connection -> IO ()
addCreateAdminKey conn = do
  _ <- execute_ conn "INSERT INTO key (create_admin_key) VALUES ( 'lola' ) "
  return ()-}
