{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module TryRead where
          
import           Oops
import           Data.Time.Calendar             ( Day, fromGregorianValid)
import           Data.Text                      ( pack, unpack, Text )
import           Control.Monad.Trans.Except (ExceptT,throwE,catchE)
import           Control.Monad (when)


tryReadNum :: (Monad m) => Text -> ExceptT ReqError m Integer
tryReadNum "" = throwE $ SimpleError "Can`t parse parameter. Empty input."
tryReadNum xs = case reads . unpack $ xs of
  [(a,"")] -> return a
  _        -> throwE $ SimpleError $ "Can`t parse value: " ++ unpack xs ++ ". It must be number"

tryReadNumArray :: (Monad m) => Text -> ExceptT ReqError m [Integer]
tryReadNumArray "" = throwE $ SimpleError "Can`t parse parameter. Empty input."
tryReadNumArray xs = case reads . unpack $ xs of
  [([],"")] -> throwE $ SimpleError $ "Can`t parse value: " ++ unpack xs ++ ". It must be NOT empty array of numbers. Example: [3,45,24,7] "
  [(a,"")]  -> return a
  _         -> throwE $ SimpleError $ "Can`t parse value: " ++ unpack xs ++ ". It must be array of numbers. Example: [3,45,24,7] "

tryReadDay :: (Monad m) => Text -> ExceptT ReqError m Day
tryReadDay "" = throwE $ SimpleError "Can`t parse parameter. Empty input."
tryReadDay xs = case filter (' ' /=) . unpack $ xs of
  [] -> throwE $ SimpleError "Empty input. Date must have format (yyyy-mm-dd). Example: 2020-12-12"
  [a, b, c, d, '-', e, f, '-', g, h] -> do
    year  <- tryReadNum (pack [a, b, c, d]) `catchE` (\(SimpleError str) -> throwE $ SimpleError (str ++ ". Date must have format (yyyy-mm-dd). Example: 2020-12-12"))
    month <- tryReadNum (pack [e, f]) `catchE` (\(SimpleError str) -> throwE $ SimpleError (str ++ ". Date must have format (yyyy-mm-dd). Example: 2020-12-12"))
    when (month `notElem` [1..12]) $ throwE $ SimpleError ("Can`t parse value: " ++ unpack xs ++ ". Month must be a number from 1 to 12. Date must have format (yyyy-mm-dd). Example: 2020-12-12")
    day   <- tryReadNum (pack [g, h]) `catchE` (\(SimpleError str) -> throwE $ SimpleError (str ++ ". Date must have format (yyyy-mm-dd). Example: 2020-12-12"))
    when (day `notElem` [1..31]) $ throwE $ SimpleError ("Can`t parse value: " ++ unpack xs ++ ". Day of month must be a number from 1 to 31. Date must have format (yyyy-mm-dd). Example: 2020-12-12")
    case fromGregorianValid year (fromInteger month) (fromInteger day) of
      Just x -> return x
      Nothing -> throwE $ SimpleError $ "Can`t parse value: " ++ unpack xs ++ ". Invalid day, month, year combination. Date must have format (yyyy-mm-dd). Example: 2020-12-12"     
  _        -> throwE $ SimpleError $ "Can`t parse value: " ++ unpack xs ++ ". Date must have format (yyyy-mm-dd). Example: 2020-12-12"