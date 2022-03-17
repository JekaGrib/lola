{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Auth.QStrExample where


import Network.HTTP.Types (QueryText)

qStr1 :: QueryText
qStr1 = [("token",Just "lola")]



