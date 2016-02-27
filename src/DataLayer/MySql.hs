module DataLayer.MySql (
   buildConnInfo,
   colQuery,
   escapeString,
   getSchema,
   testConnection,
   makeConnection
) where

import qualified Data.ByteString as BS
import Data.String (fromString)
import Control.Exception(handle, displayException, SomeException)
import Control.Monad(forM)
import Data.Text (Text(..), unpack)
import Data.Text.Encoding (decodeUtf8)

import Database.MySQL.Base (MySQLError, escape)
import Database.MySQL.Base.Types
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults

import DataLayer.Types

colQuery :: Connection -> GetByColumnTemplate -> [BS.ByteString] -> IO (Either String [ResultRow])
colQuery conn colQuery values = handle returnException $ do
   let q = ["select * from `", tableName colQuery, "` where `", columnName colQuery, "` in ?"]
   let queryType = fromString $ concat q :: Query
   results <- query conn queryType $ Only (In values) :: IO [BasicResult]
   return $ Right $ map (\(BasicResult b) -> ResultRow b) results

returnException :: SomeException -> IO (Either String a)
returnException e = return $ Left $ displayException e

escapeString :: Connection -> BS.ByteString -> IO BS.ByteString
escapeString conn input = escape conn input

getSchema :: Connection -> IO (Either String [TableSchema])
getSchema conn = do
   tables <- query_ conn (fromString "show tables") :: IO [Only BS.ByteString]
   schemas <- forM tables (makeSchema . unwrapOnly)
   return $ Right schemas
   where
      makeSchema :: BS.ByteString -> IO TableSchema
      makeSchema tableName = do
         cols <- getColumns tableName 
         return $ TableSchema tableName cols
      getColumns :: BS.ByteString -> IO [TableColumn]
      getColumns tableName = do
         tableData <- query_ conn (buildDescQuery tableName) :: IO [BasicResult]
         return $ map (TableColumn . getField) tableData
      getField (BasicResult row) = case lookup (fromString "Field") row of
              Just (Just fieldName) -> fieldName
              Just (Nothing) -> error "Null entry in desc"
              Nothing -> error "Couldn't find Field column in desc"
      buildDescQuery tableName = fromString $ "desc " ++ (unpack $ decodeUtf8 tableName)
      unwrapOnly (Only i) = i

testConnection :: Connection -> IO Bool
testConnection conn = handle handleException $ do
   rows <- query_ conn (fromString "select 1") :: IO [BasicResult]
   return $ (length rows) == 1
   where
      handleException :: MySQLError -> IO Bool
      handleException e = return False

makeConnection :: ConnectInfo -> IO (Either String Connection)
makeConnection connInfo = handle returnException $ fmap Right $ connect connInfo

type InputConfig = [(String, String)]
buildConnInfo :: InputConfig -> Either String ConnectInfo
buildConnInfo conf = Right $ foldl applyConfItem defaultConnectInfo conf
   where
      applyConfItem confObj ("username", val) = confObj { connectUser = val }
      applyConfItem confObj ("password", val) = confObj { connectPassword = val }
      applyConfItem confObj ("dbname", val) = confObj { connectDatabase = val }
      applyConfItem confObj ("path", val) = confObj { connectPath = val }
      applyConfItem confObj ("port", val) = confObj { connectPort = (read val) }
      applyConfItem confObj _ = confObj

newtype BasicResult = BasicResult [(BS.ByteString, Maybe BS.ByteString)] deriving Show
instance QueryResults BasicResult where
   convertResults fields values = BasicResult $ zipWith (\f v -> (fieldName f, v)) fields values

