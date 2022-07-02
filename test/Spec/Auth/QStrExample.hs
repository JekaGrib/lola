module Spec.Auth.QStrExample where

import Network.HTTP.Types (QueryText)

qStr1 :: QueryText
qStr1 = [("token", Just "152.hij.83a6f25e7d42750b6b1769efa231cccbfd8d319c")]

qStr2 :: QueryText
qStr2 = [("token", Just "oops")]

qStr3 :: QueryText
qStr3 = [("token", Just "152.hij.oops")]

qStr4 :: QueryText
qStr4 = [("token", Just "152.stu.b6ddb16b91f61489aeebabf2a9da48f219e10ca7")]
