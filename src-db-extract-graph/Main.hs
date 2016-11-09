import System.Environment (getArgs)
import System.Console.GetOpt as GO
import System.Exit(exitSuccess, exitWith, ExitCode(ExitFailure))
import qualified Data.Yaml as YAML

import Config (Config(..), ForeignKey(..))

main = do
   rawArgs <- getArgs
   case GO.getOpt GO.Permute [] rawArgs of
        ([], [configPath], []) -> processInput configPath
        ([], _, []) -> exitWithError ""
        (_, _, errs) -> exitWithError $ concat errs
   where
      processInput configPath = do
         eitherContents <- YAML.decodeFileEither configPath
         case eitherContents of
              Left ex -> exitWithError $ "Problem with config file: " ++ YAML.prettyPrintParseException ex ++ "\n"
              Right config -> do
                  let dotInfo = renderDot $ configReferences config
                  putStrLn dotInfo

exitWithError s = putStr (s ++ usage) >> (exitWith $ ExitFailure 1)

usage = "Usage: db-extract <database-config.yml>\n\nRenders the connections included in the given config file to the dot language which can be drawn by graphviz.\n"

renderDot :: [ForeignKey] -> String
renderDot fks = start ++ connections ++ end
    where
        connections = concat $ map renderConn fks
        renderConn (ForeignKey subject subjectCol object objectCol unidirectional) =
            case unidirectional of
                 True -> concat [
                        "  ", subject, "->", object, "[label=\"", subjectCol, "\"];\n"
                    ]
                 False -> concat [
                        "  ", subject, "->", object, "[label=\"", subjectCol, "\"];\n",
                        "  ", object, "->", subject, "[label=\"", objectCol, "\"];\n"
                    ]
        start = "digraph {\n"
        end = "}"
