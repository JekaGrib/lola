module Psql.Methods.Admin where

import Database.PostgreSQL.Simple (Connection)
import Psql.Methods.Common
import Psql.ToQuery.Insert (InsertPair (..), InsertRet (..))
import Psql.ToQuery.Select (Select (..), Where (..))
import Types

selectKeys' :: Connection -> IO [Key]
selectKeys' conn =
  selectOnly' conn $ Select ["create_admin_key"] "key" (Where "true")

insertReturnUser' :: Connection -> InsertUser -> IO UserId
insertReturnUser' conn (InsertUser pwd fName lName picId day bool tokenKey) = do
  let insPairPwd = InsertPair "password" (Txt pwd)
      insPairFirstName = InsertPair "first_name" (Txt fName)
      insPairLastName = InsertPair "last_name" (Txt lName)
      insPairPic = InsertPair "user_pic_id" (Id picId)
      insPairDay = InsertPair "user_create_date" (Day day)
      insPairAdmin = InsertPair "admin" (Bool bool)
      insPairKey = InsertPair "token_key" (Str tokenKey)
      insPairs =
        [ insPairPwd,
          insPairFirstName,
          insPairLastName,
          insPairPic,
          insPairDay,
          insPairAdmin,
          insPairKey
        ]
  insertReturn' conn (InsertRet "users" insPairs "user_id")
