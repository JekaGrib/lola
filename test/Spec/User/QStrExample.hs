{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.User.QStrExample where

import Network.HTTP.Types (QueryText)

qStr1 :: QueryText
qStr1 = [("token", Just "152.hij.83a6f25e7d42750b6b1769efa231cccbfd8d319c")]

qStr2 :: QueryText
qStr2 = [("tag_name", Just "dogs"), ("token", Just "152.hij.83a6f25e7d42750b6b1769efa231cccbfd8d319c")]

qStr3 :: QueryText
qStr3 = [("user_id", Just "4"),("password", Just "pwd")]

qStr4 :: QueryText
qStr4 = [("user_id", Just "200"),("password", Just "pwd")]