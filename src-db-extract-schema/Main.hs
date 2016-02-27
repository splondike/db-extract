import System.Environment (getArgs)
import System.Console.GetOpt as GO
import System.Exit(exitSuccess, exitWith, ExitCode(ExitFailure))
import Data.Set(toList)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Yaml as YAML

import qualified Config as C
import qualified DataLayer as DL
import qualified ExtractSchema.Program as P

main = do
   rawArgs <- getArgs
   case GO.getOpt GO.Permute options rawArgs of
        ([configPath], [], []) -> processInput configPath
        ([], _, []) -> exitWithError ""
        (_, _, errs) -> exitWithError $ concat errs
   where
      processInput configPath = do
         eitherContents <- YAML.decodeFileEither configPath
         case eitherContents of
              Left ex -> exitWithError $ "Problem with config file: " ++ YAML.prettyPrintParseException ex ++ "\n"
              Right config -> buildDb config >>= \db->
                 buildNewConfig db config

      buildNewConfig db config = do
         foreignKeys <- P.processDatabase db
         let newConfig = config{C.configReferences = toList foreignKeys}
         putStrLn $ unpack . decodeUtf8 $ YAML.encode newConfig

      buildDb config = DL.buildFetcher dbType dbParams >>= \fetcher -> case fetcher of
                            Left err -> error err
                            Right db -> return db
         where 
            dbType = (C.databaseType dbConf)
            dbParams = (C.databaseParams dbConf)
            dbConf = C.configDatabase config

exitWithError s = putStr (s ++ usage) >> (exitWith $ ExitFailure 1)

options = [config]
   where
      config = GO.Option "-c" ["config"] (GO.ReqArg id "config.yml") "The configuration file containing DB connection and foreign key information."

usage = GO.usageInfo "Usage: db-extract --config <database-config.yml>\n\nBuild a config file containing deduced reference information for the database described in the given config" options
