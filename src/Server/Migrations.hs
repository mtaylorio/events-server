module Server.Migrations
  ( runDatabaseMigrations
  ) where

import Hasql.Migration
import Hasql.Transaction.Sessions
import System.Exit
import System.IO
import qualified Hasql.Pool as Pool


runDatabaseMigrations :: FilePath -> Pool.Pool -> IO ()
runDatabaseMigrations migrations pool = do
  putStrLn "Running migrations"
  hFlush stdout

  commands <- loadMigrationsFromDirectory migrations
  let migrationCommands = MigrationInitialization : commands
  runDatabaseMigrations' migrationCommands pool

  putStrLn "Done running migrations"
  hFlush stdout

  return ()


runDatabaseMigrations' :: [MigrationCommand] -> Pool.Pool -> IO ()
runDatabaseMigrations' [] _ = return ()
runDatabaseMigrations' (command:commands) pool = do
  let tr = runMigration command
  let session = transaction Serializable Write tr

  result <- Pool.use pool session
  case result of
    Left err -> do
      putStrLn $ "Error running migration: " ++ show err
      hFlush stdout
      exitFailure
    Right migrationResult ->
      case migrationResult of
        Nothing -> runDatabaseMigrations' commands pool
        Just err -> do
          putStrLn $ "Error running migration: " ++ show err
          hFlush stdout
          exitFailure
