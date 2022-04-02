{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Admin.QStrExample where

import Network.HTTP.Types (QueryText)


qStr1 :: QueryText
qStr1 = [("create_admin_key", Just "lola"),("first_name", Just "fName"),("last_name", Just "lName"),("password", Just "pwd"),("user_pic_id", Just "6")]

