module Spec.Draft.QStrExample where

import Network.HTTP.Types (QueryText)

qStr1 :: QueryText
qStr1 = [("token", Just "3.hij.83a6f25e7d42750b6b1769efa231cccbfd8d319c")]

qStr2 :: QueryText
qStr2 =
  [ ("page", Just "1"),
    ("token", Just "4.hij.83a6f25e7d42750b6b1769efa231cccbfd8d319c")
  ]

qStr3 :: QueryText
qStr3 =
  [ ("comment_text", Just "yes"),
    ("token", Just "152.hij.83a6f25e7d42750b6b1769efa231cccbfd8d319c")
  ]

qStr4 :: QueryText
qStr4 = [("post_id", Just "7"), ("page", Just "1")]
