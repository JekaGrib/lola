module Spec.Post.QStrExample where

import Network.HTTP.Types (QueryText)

qStr1 :: QueryText
qStr1 = [("token", Just "3.hij.83a6f25e7d42750b6b1769efa231cccbfd8d319c")]

qStr2 :: QueryText
qStr2 =
  [ ("author_info", Just "author"),
    ("user_id", Just "3"),
    ("token", Just "152.hij.83a6f25e7d42750b6b1769efa231cccbfd8d319c")
  ]

qStr3 :: QueryText
qStr3 = [("page", Just "1")]
