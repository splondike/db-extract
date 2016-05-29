module DataLayer.MySql (
   buildConnInfo,
   colQuery,
   escapeString,
   getSchema,
   testConnection,
   makeConnection,
   buckets
) where

import qualified Data.ByteString as BS
import Data.String (fromString)
import Control.Exception(handle, displayException, SomeException)
import Control.Monad(forM)
import Data.List (groupBy, sort)
import Data.Maybe (fromJust)
import Data.Text (Text(..), unpack)
import Data.Text.Encoding (decodeUtf8)

import Database.MySQL.Base (MySQLError, escape)
import Database.MySQL.Base.Types
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults

import DataLayer.Types

colQuery :: MySqlConnection -> GetByColumnTemplate -> [BS.ByteString] -> IO (Either String [ResultRow])
colQuery mysqlCon colQuery values = handle returnException $ do
   let conn = getConnection mysqlCon
   let q = ["select * from `", tableName colQuery, "` where `", columnName colQuery, "` in ?"]
   let queryType = fromString $ concat q :: Query
   results <- query conn queryType $ Only (In values) :: IO [BasicResult]
   return $ Right $ map (\(BasicResult b) -> ResultRow b) results

returnException :: SomeException -> IO (Either String a)
returnException e = return $ Left $ displayException e

escapeString :: MySqlConnection -> BS.ByteString -> IO BS.ByteString
escapeString mysqlCon input = escape (getConnection mysqlCon) input

getSchema :: MySqlConnection -> IO (Either String [TableSchema])
getSchema mysqlCon = handle returnException $ do
   cols <- query conn (fromString colQuery) (Only dbName) :: IO [BasicResult]
   constraints <- query conn (fromString fkQuery) (dbName, dbName) :: IO [BasicResult]
   let tableSchemas = buckets getTableName $ map unwrapBasicResult cols
       tableConstraints = buckets getTableName $ map unwrapBasicResult constraints
   return $ Right $ buildSchemas tableSchemas tableConstraints
   where
      buildSchemas tableSchemas tableConstraints = map (buildSchema tableConstraints) tableSchemas
      buildSchema tableConstraints (table, cols) = TableSchema table (map mkTableCol cols) fks
         where
            mkTableCol row = TableColumn $ fromJust $ fromJust $
               lookup (fromString "column_name") row
            fks = case lookup table tableConstraints of
                       Just constraints -> map rowToTableFk constraints
                       Nothing -> []
      rowToTableFk row = TableFKConstraint (locate "column_name") (locate "referenced_table_name") (locate "referenced_column_name")
         where
            locate name = fromJust $ fromJust $ lookup (fromString name) row


      getTableName pairs = fromJust $ fromJust $ lookup (fromString "table_name") pairs
      colQuery = "SELECT table_name, column_name FROM information_schema.columns WHERE table_schema = ? ORDER BY table_name"
      fkQuery = "SELECT table_name, column_name, referenced_table_name, referenced_column_name FROM information_schema.key_column_usage WHERE table_schema = ? AND referenced_table_schema = ? AND referenced_column_name is not null ORDER BY table_name"
      unwrapBasicResult (BasicResult x) = x
      dbName = connectDatabase connInfo
      MySqlConnection (connInfo, conn) = mysqlCon

buckets :: (Ord a, Eq b) => (a -> b) -> [a] -> [(b, [a])]
buckets keyFunc xs = map makePair $ groupBy groupF (sort xs)
   where
      makePair list@(x:xs) = (keyFunc x, list)
      groupF first second = (keyFunc first) == (keyFunc second)

testConnection :: MySqlConnection -> IO Bool
testConnection mysqlCon = handle handleException $ do
   let conn = getConnection mysqlCon
   rows <- query_ conn (fromString "select 1") :: IO [BasicResult]
   return $ (length rows) == 1
   where
      handleException :: MySQLError -> IO Bool
      handleException e = return False

makeConnection :: ConnectInfo -> IO (Either String MySqlConnection)
makeConnection connInfo = handle returnException $ fmap mkPair $ connect connInfo
   where
      mkPair conn = Right $ MySqlConnection (connInfo, conn)

type InputConfig = [(String, String)]
buildConnInfo :: InputConfig -> Either String ConnectInfo
buildConnInfo conf = Right $ foldl applyConfItem defaultConnectInfo conf
   where
      applyConfItem confObj ("username", val) = confObj { connectUser = val }
      applyConfItem confObj ("password", val) = confObj { connectPassword = val }
      applyConfItem confObj ("dbname", val) = confObj { connectDatabase = val }
      applyConfItem confObj ("host", val) = confObj { connectHost = val }
      applyConfItem confObj ("path", val) = confObj { connectPath = val }
      applyConfItem confObj ("port", val) = confObj { connectPort = (read val) }
      applyConfItem confObj _ = confObj

newtype BasicResult = BasicResult [(BS.ByteString, Maybe BS.ByteString)] deriving Show
instance QueryResults BasicResult where
   convertResults fields values = BasicResult $ zipWith (\f v -> (fieldName f, v)) fields values

newtype MySqlConnection = MySqlConnection (ConnectInfo, Connection)
getConnection (MySqlConnection (_, c)) = c
