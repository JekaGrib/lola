{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module TryRead where
          
import           Oops
import Types
import           Data.Time.Calendar             ( Day, fromGregorianValid)
import           Data.Text                      ( pack, unpack, Text )
import           Control.Monad.Trans.Except (ExceptT,throwE,catchE)
import           Control.Monad (when)


tryReadNum :: (Monad m) => QueryParamKey -> Text -> ExceptT ReqError m Integer
tryReadNum paramKey "" = throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". Empty input."
tryReadNum paramKey xs = case reads . unpack $ xs of
  [(a,"")] -> return a
  _        -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". Value: " ++ unpack xs ++ ". It must be number"

tryReadId :: (Monad m) => QueryParamKey -> Text -> ExceptT ReqError m Id
tryReadId paramKey txt = do
  num <- tryReadNum paramKey txt 
  checkBigInt paramKey num

tryReadPage :: (Monad m) => Text -> ExceptT ReqError m Page
tryReadPage txt = do
  num <- tryReadNum "page" txt 
  checkPage num

tryReadNumArray :: (Monad m) => QueryParamKey -> Text -> ExceptT ReqError m [Integer]
tryReadNumArray paramKey "" = throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ " Empty input."
tryReadNumArray paramKey xs = case reads . unpack $ xs of
  [([],"")] -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It must be NOT empty array of numbers. Example: [3,45,24,7] "
  [(a,"")]  -> return a
  _         -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It must be array of numbers. Example: [3,45,24,7] "

tryReadIdArray :: (Monad m) => QueryParamKey -> Text -> ExceptT ReqError m [Id]
tryReadIdArray paramKey xs = do
  nums <- tryReadNumArray paramKey xs
  mapM (checkBigInt paramKey) nums

tryReadDay :: (Monad m) => QueryParamKey -> Text -> ExceptT ReqError m Day
tryReadDay paramKey "" = throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ " Empty input."
tryReadDay paramKey xs = case filter (' ' /=) . unpack $ xs of
  [] -> throwE $ SimpleError "Empty input. Date must have format (yyyy-mm-dd). Example: 2020-12-12"
  [a, b, c, d, '-', e, f, '-', g, h] -> do
    year  <- tryReadNum paramKey (pack [a, b, c, d]) `catchE` (\(SimpleError str) -> throwE $ SimpleError (str ++ ". Date must have format (yyyy-mm-dd). Example: 2020-12-12"))
    month <- tryReadNum paramKey (pack [e, f]) `catchE` (\(SimpleError str) -> throwE $ SimpleError (str ++ ". Date must have format (yyyy-mm-dd). Example: 2020-12-12"))
    when (month `notElem` [1..12]) $ throwE $ SimpleError ("Can`t parse parameter: " ++ unpack paramKey ++ ". Month must be a number from 1 to 12. Date must have format (yyyy-mm-dd). Example: 2020-12-12")
    day  <- tryReadNum paramKey (pack [g, h]) `catchE` (\(SimpleError str) -> throwE $ SimpleError (str ++ ". Date must have format (yyyy-mm-dd). Example: 2020-12-12"))
    when (day `notElem` [1..31]) $ throwE $ SimpleError ("Can`t parse parameter: " ++ unpack paramKey ++ ". Day of month must be a number from 1 to 31. Date must have format (yyyy-mm-dd). Example: 2020-12-12")
    case fromGregorianValid year (fromInteger month) (fromInteger day) of
      Just x -> return x
      Nothing -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". Value: " ++ unpack xs ++ ". Invalid day, month, year combination. Date must have format (yyyy-mm-dd). Example: 2020-12-12"     
  _        -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". Value: " ++ unpack xs ++ ". Date must have format (yyyy-mm-dd). Example: 2020-12-12"




checkBigInt :: (Monad m) => QueryParamKey -> Id -> ExceptT ReqError m Id
checkBigInt paramKey num 
  | num <= 0 = throwE $ SimpleError $ "Parameter: " ++ unpack paramKey ++ " . Value should be greater then 0"
  | num > 9223372036854775805 = throwE $ SimpleError $ "Parameter: " ++ unpack paramKey ++ ". Value should be less then 9223372036854775805"
  | otherwise = return num

checkPage :: (Monad m) => Integer -> ExceptT ReqError m Page
checkPage num 
  | num <= 0 = throwE $ SimpleError $ "Page should be greater then 0"
  | num > 100000 = throwE $ SimpleError $ "Page should be less then 100000"
  | otherwise = return (fromInteger num)

{-
tryReadNum :: (Monad m) => Text -> ExceptT ReqError m Integer
tryReadNum "" = throwE $ SimpleError "Can`t parse parameter. Empty input."
tryReadNum xs = case reads . unpack $ xs of
  [(a,"")] -> return a
  _        -> throwE $ SimpleError $ "Can`t parse value: " ++ unpack xs ++ ". It must be number"-}