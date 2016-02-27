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
import qualified DataLayer.Types as DT (TableSchema(..), TableColumn(..))

processDatabase :: DL.DBFetcher -> IO (S.Set ForeignKey)
processDatabase db = fetchSchema >>= return . map toTableDef >>= return . findFks
   where
      fetchSchema = DL.getSchema db >>= \schemaE ->
         case schemaE of
           Right schema -> return schema
           Left err -> error err
      toTableDef schema = (toS $ DT.schemaName schema, map (toS . DT.name) $ DT.columns schema)
      toS :: BS.ByteString -> String
      toS = unpack . decodeUtf8
      findFks :: [TableDef] -> S.Set ForeignKey
      findFks schemaMap = foldl (processSchema schemaMap) S.empty schemaMap

processSchema :: [TableDef] -> S.Set ForeignKey -> TableDef -> S.Set ForeignKey
processSchema schemaMap rtn (tableName, cols) = newRtn
   where
      newRtn = foldl addNewFk rtn colTableTuples
      addNewFk set tuple = S.insert (makeFk tuple) set
      makeFk (localCol, foreignTableName, foreignTableCol) = 
         ForeignKey tableName localCol foreignTableName foreignTableCol

      colTableTuples :: [FKRef]
      colTableTuples = foldl addMatchingColumns [] cols

      addMatchingColumns rtn col = case findMatchingCol schemaMap col of
                                        Just match -> match:rtn
                                        Nothing -> rtn

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

toLowerS = map toLower

type TableDef = (String, [String])
type FKRef = (String, String, String)
-- ^ (local col, e.g. author_id, foreign table, e.g. authors, foreign col, e.g. id)
