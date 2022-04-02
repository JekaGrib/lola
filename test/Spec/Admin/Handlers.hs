{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Admin.Handlers where

import Control.Monad.State (StateT (..), modify)
import Methods.Admin
import Spec.Conf (defConf)
import qualified Spec.Exist.Handlers (handle)
import Spec.Log (handLogWarning)
import Spec.Admin.Types
import Spec.Types (MockAction (..))
import Types
import Data.Time.Calendar (Day,fromGregorian)




handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogWarning
    selectKeysTest
    insertReturnUserTest
    getDayTest
    generateTokenKeyTest
    Spec.Exist.Handlers.handle

selectKeysTest :: StateT [MockAction] IO [Key]
selectKeysTest = do
  modify (AdminMock SelectKeys :)
  return ["lola"]


insertReturnUserTest :: InsertUser -> StateT [MockAction] IO UserId
insertReturnUserTest insUs = do
  modify (AdminMock (InsertReturnUser insUs) :)
  return 14

getDayTest :: StateT [MockAction] IO Day
getDayTest =  do
  modify (AdminMock GetDay :)
  return dayExample

dayExample :: Day
dayExample = (fromGregorian 2020 02 02)

generateTokenKeyTest :: StateT [MockAction] IO TokenKey
generateTokenKeyTest = do
  modify (AdminMock GenerateTokenKey :)
  return "lilu"


