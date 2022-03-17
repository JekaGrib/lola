{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Exist.QStrExample where


import Network.HTTP.Types (QueryText)


qStr1 :: QueryText
qStr1 = [("token",Just "lola")]



