module ExtractSchema.Program (
   processDatabase
) where

import Data.Maybe(catMaybes, listToMaybe)
import Data.Char(toLower)
import Data.List(isSuffixOf, break, tails, intercalate, splitAt, find)
import Data.String(fromString)
import Data.Text (Text(..), unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.Set as S

import Config (ForeignKey(..))
import qualified DataLayer as DL
import qualified DataLayer.Types as DT (TableSchema(..), TableColumn(..), TableFKConstraint(..))

processDatabase :: DL.DBFetcher -> Bool -> IO (S.Set ForeignKey)
processDatabase db inferFks = fetchSchemas >>= return . findFks
   where
      fetchSchemas = DL.getSchema db >>= \schemasE ->
         case schemasE of
           Right schemas -> return schemas
           Left err -> error err

      findFks schemas = S.union (explicitFks schemas) (inferredFks schemas)

      explicitFks schemas = foldToSet (const extractExplicitFks) schemas

      inferredFks schemas
         | inferFks = foldToSet inferrFksFromCols schemas
         | otherwise = S.empty

      foldToSet f schemas = foldl (\c i -> S.union c (f schemas i)) S.empty schemas

extractExplicitFks :: DT.TableSchema -> S.Set ForeignKey
extractExplicitFks schema = S.fromList $ map makeFk fks
   where
      fks = DT.foreignKeys schema
      makeFk (DT.TableFKConstraint lc rt rc) = 
         ForeignKey localTable (toS lc) (toS rt) (toS rc) True
      localTable = toS $ DT.schemaName schema

inferrFksFromCols :: [DT.TableSchema] -> DT.TableSchema -> S.Set ForeignKey
inferrFksFromCols schemas currSchema = foldl addNewFk S.empty colTableTuples
   where
      addNewFk set tuple = S.insert (makeFk tuple) set
      makeFk (localCol, foreignTableName, foreignTableCol) = 
         ForeignKey tableName localCol foreignTableName foreignTableCol True

      colTableTuples :: [FKRef]
      colTableTuples = foldl addMatchingColumns [] cols

      addMatchingColumns rtn col = case findMatchingCol schemaMap col of
                                        Just match -> match:rtn
                                        Nothing -> rtn

      (tableName, cols) = toTableDef currSchema
      schemaMap = map toTableDef schemas
      toTableDef schema = (toS $ DT.schemaName schema, map (toS . DT.name) $ DT.columns schema)

-- | Find the best match for the column in the list of schemas
findMatchingCol :: [TableDef] -- ^ The schemas to search for a match in
                   -> String -- ^ The column to find a match for
                   -> Maybe FKRef -- ^ Just if we could find a nice match, Nothing otherwise
findMatchingCol schemaMap col = listToMaybe guesses
   where
      guesses = catMaybes $ map maybeFkRef tableGuesses
      maybeFkRef tableName = case lookup tableName schemaMap >>= findIdCol of
                            Just idCol -> Just (col, tableName, idCol)
                            Nothing -> Nothing

      findIdCol tableCols = find (\col -> toLowerS col == "id") tableCols

      -- | Produce a list of table names that would match in priority order
      tableGuesses = guessTables (map fst schemaMap) col

-- | Guesses which of the tables in the first argument are a good match for the 
-- column name in the second argument, returns the guesses in priority order
guessTables :: [String] -> String -> [String]
guessTables tableNames col
   | isSuffixOf "id" (toLowerS col) = rtn
   | otherwise = []
   where
      rtn = filter isTable $ concat $ map possibleNames tableNameTemplates
      isTable name = any (==name) tableNames
      -- | Permutes a template by pluralizing, keeping singular, or converting
      -- a trailing y to ies
      possibleNames template = concat $ map (\f -> f template) templateExpanders
      templateExpanders = [(\t -> [t ++ "s"]), (\t -> [t]), pluralizeTrailingY]
      pluralizeTrailingY template = case isSuffixOf "y" template of
                                True -> [(reverse $ drop 1 $ reverse template) ++ "ies"]
                                False -> []
      -- | Turns ["primary", "author"] into ["primary_author", "author"]
      tableNameTemplates = map (intercalate "_") $ tails colParts
      -- | Split on underscore and drop the final 'id' part
      colParts = reverse $ drop 1 $ reverse $ split '_' col

split :: (Eq a, Show a) => a -> [a] -> [[a]]
split delim xs = reverse $ split' [] xs
   where
      split' acc rest = process acc $ break (==delim) rest
      process acc (pre, []) = pre:acc
      process acc (pre, post) = split' (pre:acc) (tail post)

toLowerS :: String -> String
toLowerS = map toLower

toS :: BS.ByteString -> String
toS = unpack . decodeUtf8

type TableDef = (String, [String])
type FKRef = (String, String, String)
-- ^ (local col, e.g. author_id, foreign table, e.g. authors, foreign col, e.g. id)
