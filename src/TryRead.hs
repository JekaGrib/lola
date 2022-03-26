{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module TryRead where

import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Except (ExceptT, catchE, throwE)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorianValid)
import Oops
import Types

tryReadInteger :: (Monad m) => QueryParamKey -> Text -> ExceptT ReqError m Integer
tryReadInteger paramKey "" = throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". Empty input."
tryReadInteger paramKey txt = case reads . unpack $ txt of
  [(a, "")] -> return a
  _ -> throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". Value: " ++ getTxtstart txt ++ ". It must be whole number"

tryReadId :: (Monad m) => QueryParamKey -> Text -> ExceptT ReqError m Id
tryReadId paramKey txt = do
  num <- tryReadInteger paramKey txt
  checkBigIntId paramKey num

tryReadResourseId :: (MonadCatch m) => QueryParamKey -> Text -> ExceptT ReqError m Id
tryReadResourseId paramKey txt =
  hideResourseNotExistErr $
    tryReadId paramKey txt

tryReadPage :: (Monad m) => Text -> ExceptT ReqError m Page
tryReadPage txt = do
  num <- tryReadInteger "page" txt
  checkPage num

tryReadNumArray :: (Monad m) => QueryParamKey -> Text -> ExceptT ReqError m [Integer]
tryReadNumArray paramKey "" = throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ " Empty input."
tryReadNumArray paramKey txt = case reads . unpack $ txt of
  [([], "")] -> throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It must be NOT empty array of numbers. Example: [3,45,24,7] "
  [(a, "")] -> return a
  _ -> throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It must be array of numbers. Example: [3,45,24,7] "

tryReadIdArray :: (Monad m) => QueryParamKey -> Text -> ExceptT ReqError m [Id]
tryReadIdArray paramKey txt = do
  nums <- tryReadNumArray paramKey txt
  mapM (checkBigIntId paramKey) nums

tryReadDay :: (Monad m) => QueryParamKey -> Text -> ExceptT ReqError m Day
tryReadDay paramKey "" = throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ " Empty input."
tryReadDay paramKey txt = do
  let infoErrStr = ". Date must have format (yyyy-mm-dd). Example: 2020-12-12"
  case filter (' ' /=) . unpack $ txt of
    [] -> throwE $ BadReqError $ "Empty input." ++ infoErrStr
    [a, b, c, d, '-', e, f, '-', g, h] -> do
      year <- tryReadInteger paramKey (pack [a, b, c, d]) `catchE` addToBadReqErr infoErrStr
      month <- tryReadInteger paramKey (pack [e, f]) `catchE` addToBadReqErr infoErrStr
      when (month `notElem` [1 .. 12]) $ throwE $ BadReqError ("Can`t parse parameter: " ++ unpack paramKey ++ ". Month must be a number from 1 to 12." ++ infoErrStr)
      day <- tryReadInteger paramKey (pack [g, h]) `catchE` addToBadReqErr infoErrStr
      when (day `notElem` [1 .. 31]) $ throwE $ BadReqError ("Can`t parse parameter: " ++ unpack paramKey ++ ". Day of month must be a number from 1 to 31." ++ infoErrStr)
      case fromGregorianValid year (fromInteger month) (fromInteger day) of
        Just x -> return x
        Nothing -> throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". Value: " ++ unpack txt ++ ". Invalid day, month, year combination." ++ infoErrStr
    _ -> throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ infoErrStr

tryReadSortOrd :: (Monad m) => QueryParamKey -> Text -> ExceptT ReqError m SortOrd
tryReadSortOrd paramKey "" = throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". Empty input."
tryReadSortOrd paramKey txt = case T.toUpper . T.take 4 $ txt of
  "ASC" -> return ASC
  "DESC" -> return DESC
  _ -> throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". Value: " ++ getTxtstart txt ++ ". It must be <ASC> or <DESC>"

checkBigIntId :: (Monad m) => QueryParamKey -> Integer -> ExceptT ReqError m Id
checkBigIntId paramKey num
  | num <= 0 = throwE $ BadReqError $ "Parameter: " ++ unpack paramKey ++ " . Id should be greater then 0"
  | num > 9223372036854775805 = throwE $ BadReqError $ "Parameter: " ++ unpack paramKey ++ ". Id should be less then 9223372036854775805"
  | otherwise = return (fromInteger num)

checkPage :: (Monad m) => Integer -> ExceptT ReqError m Page
checkPage num
  | num <= 0 = throwE $ BadReqError "Page should be greater then 0"
  | num > 100000 = throwE $ BadReqError "Page should be less then 100000"
  | otherwise = return (fromInteger num)

getTxtstart :: Text -> String
getTxtstart txt = case splitAt 20 (unpack txt) of
  (str, []) -> str
  (str, _) -> str ++ "... "

{-checkIdLength :: (Monad m) => Text -> ExceptT ReqError m ()
checkIdLength leng txt = case splitAt 20 (unpack txt) of
  (_, []) -> return ()
  _ -> throwE $ SecretTokenError $ "Token too long. Maximum length should be: " ++ show leng

tryReadNum :: (Monad m) => Text -> ExceptT ReqError m Integer
tryReadNum "" = throwE $ SimpleError "Can`t parse parameter. Empty input."
tryReadNum xs = case reads . unpack $ xs of
  [(a,"")] -> return a
  _        -> throwE $ SimpleError $ "Can`t parse value: " ++ unpack xs ++ ". It must be number"-}
