module Psql.Migration where

import Control.Exception (SomeException, catch, throw)
import Control.Exception.Safe (throwString)
import Data.Char (toLower)
import Data.List (isPrefixOf, sort)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand
      ( MigrationDirectory,
        MigrationInitialization,
        MigrationFile
      ),
    MigrationResult (..),
    runMigrations,
    getMigrations
  )
import Error (MigrationException (..))
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import Control.Monad (when) 

data Migrate = Migrate 
  { structureMigrate :: Bool,
    testMigrate :: Bool,
    migrationsMigrate :: Bool
  }
  deriving (Eq, Show)

emptyMigrate :: Migrate
emptyMigrate = Migrate False False False  

migrateAll :: Connection -> Migrate -> IO ()
migrateAll conn (Migrate structureBool testBool migBool) = do
  when (structureBool || testBool || migBool) $ do
    let addCmds = (addStructureCommand structureBool) ++ (addTestCommand testBool) ++ (addMigCommand migBool)
    migrate addCmds conn

migrate :: [MigrationCommand] -> Connection -> IO ()
migrate addCmds conn = catchMigrationEx $ do
  result <- withTransaction conn $ runMigrations False conn cmds
  case result of
    MigrationError err -> do
      throw $ MigrationException err
    _ -> do
      putStrLn "Migrations history:"
      getMigrations conn >>= mapM_ (putStrLn . show)
  where
    cmds = MigrationInitialization : addCmds


addStructureCommand,addTestCommand,addMigCommand :: Bool -> [MigrationCommand]
addStructureCommand True = [structureMigrationCommand]
addStructureCommand _ = []
addTestCommand True = [testMigrationCommand]
addTestCommand _ = []
addMigCommand True = [migrationsCommand]
addMigCommand _ = []

structureMigrationCommand, testMigrationCommand, migrationsCommand :: MigrationCommand
structureMigrationCommand = MigrationFile "dbStructure.sql" "./dbStructure.sql"
testMigrationCommand = MigrationDirectory "./testMigrations"
migrationsCommand = MigrationDirectory "./migrations"


catchMigrationEx :: IO () -> IO ()
catchMigrationEx m = m `catch` (\e -> throw $ MigrationException (show (e :: SomeException)))

readMigrateArg :: IO Migrate
readMigrateArg = do
  args <- getArgs
  migrateArgList <- mapM fromArg . take 5 . map parseArg $ args
  return $ parseArgList migrateArgList

fromArg :: Arg -> IO MigrateArg
fromArg (Arg mArg)  = return mArg 
fromArg (UnknownArg arg)  = throwString $ unKnownArgErrMsg arg

parseArgList :: [MigrateArg] -> Migrate
parseArgList = foldr (\arg acc -> case arg of
  MigrateArg -> acc {migrationsMigrate = True}
  StructureMigrateArg -> acc {structureMigrate = True}
  TestMigrateArg -> acc {testMigrate = True}
  ) emptyMigrate


unKnownArgErrMsg :: String -> String
unKnownArgErrMsg str =
  "Error. Unknown argument in program's command line: \"" ++ str
    ++ "\".\nIf you want execute migrations, use argument - <migrate>"


data Arg = Arg MigrateArg | UnknownArg String

data MigrateArg = MigrateArg | StructureMigrateArg | TestMigrateArg 

parseArg :: String -> Arg
parseArg arg = case map toLower arg of
  "migrate" -> Arg MigrateArg
  "m" -> Arg MigrateArg
  "structuremigrate" -> Arg StructureMigrateArg
  "sm" -> Arg StructureMigrateArg
  "testmigrate" -> Arg TestMigrateArg
  "tm" -> Arg TestMigrateArg
  _ -> UnknownArg arg

scriptsInDirectory :: FilePath -> IO [String]
scriptsInDirectory dir =
  fmap
    (sort . filter (\x -> not $ "." `isPrefixOf` x))
    (getDirectoryContents dir)

