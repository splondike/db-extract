import System.Environment (getArgs)
import System.Console.GetOpt as GO
import System.Exit(exitSuccess, exitWith, ExitCode(ExitFailure))
import Data.String (fromString)
import Data.Text(unpack)
import qualified Data.Yaml as YAML

import qualified DataLayer as DL
import qualified ExtractData.Program as P
import Config (Config(..), DatabaseConfig(..))
import DataLayer.Types(GetByColumnTemplate(..), ResultRow(..))
import ExtractData.Types (Query(..))
import ExtractData.SqlRenderer (renderResults)

main = do
   rawArgs <- getArgs
   case GO.getOpt GO.Permute options rawArgs of
        ([configPath], [tableName, columnName, columnValue], []) ->
           let initialQuery = Query (GetByColumnTemplate columnName tableName) (fromString columnValue)
           in processInput configPath initialQuery
        ([], _, []) -> exitWithError ""
        (_, _, errs) -> exitWithError $ concat errs
   where
      processInput configPath initialQuery = do
         eitherContents <- YAML.decodeFileEither configPath
         case eitherContents of
              Left ex -> exitWithError $ "Problem with config file: " ++ YAML.prettyPrintParseException ex ++ "\n"
              Right config -> buildDb config >>= \db->
                 executeQuery db (configReferences config) initialQuery

      executeQuery db foreignKeys initialQuery = do
         results <- P.processQuery db initialQuery foreignKeys
         resultsSql <- renderResults db results
         putStrLn $ unpack resultsSql

      buildDb config = DL.buildFetcher dbType dbParams >>= \fetcher -> case fetcher of
                            Left err -> error err
                            Right db -> return db
         where 
            dbType = (databaseType dbConf)
            dbParams = (databaseParams dbConf)
            dbConf = configDatabase config

exitWithError s = putStr (s ++ usage) >> (exitWith $ ExitFailure 1)

options = [config]
   where
      config = GO.Option "-c" ["config"] (GO.ReqArg id "config.yml") "The configuration file containing DB connection and foreign key information."

usage = GO.usageInfo "Usage: db-extract --config <database-config.yml> <tableName> <columnName> <columnValue>\n\nRecursively fetches the foreign key dependencies of the row identified by tableName columnName and columnValue. Connection and foreign key information is passed in via the -c parameter." options
