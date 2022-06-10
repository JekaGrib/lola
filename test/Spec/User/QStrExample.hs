module Spec.User.QStrExample where

import Network.HTTP.Types (QueryText)

qStr1 :: QueryText
qStr1 = [("token", Just "152.hij.83a6f25e7d42750b6b1769efa231cccbfd8d319c")]

qStr2 :: QueryText
qStr2 = [("first_name", Just "fName"), ("last_name", Just "lName"), ("password", Just "pwd"), ("user_pic_id", Just "4")]

qStr3 :: QueryText
qStr3 = [("user_id", Just "4"), ("password", Just "pwd")]

qStr4 :: QueryText
qStr4 = [("user_id", Just "200"), ("password", Just "pwd")]

qStr5 :: QueryText
qStr5 = [("first_name", Just "fName"), ("last_name", Just "lName"), ("password", Just "pwd"), ("user_pic_id", Just "200")]
