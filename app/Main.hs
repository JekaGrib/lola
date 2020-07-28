{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.HTTP.Types (status200, status404)
import Network.Wai.Handler.Warp (run)
import Data.Aeson
import Data.Text
import Data.ByteString.Builder (lazyByteString)
import Database.PostgreSQL.Simple

createAuthorsTable :: IO ()
createAuthorsTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE authors ( author_id BIGSERIAL PRIMARY KEY NOT NULL, author_info VARCHAR(1000))"
  print "kk"

createUsersTable :: IO ()
createUsersTable = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn "CREATE TABLE users ( user_id BIGSERIAL PRIMARY KEY NOT NULL, first_name VARCHAR(50) NOT NULL, last_name  VARCHAR(50) NOT NULL, user_pic_id BIGINT NOT NULL, user_create_date DATE NOT NULL, admin boolean NOT NULL, author_id BIGINT REFERENCES authors(author_id), UNIQUE (author_id))"
  print "kk"


main :: IO ()
main = do
  createAuthorsTable
  createUsersTable
