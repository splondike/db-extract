module DataLayer (
   DBFetcher(..),
   buildFetcher
) where

import qualified Data.ByteString as BS

import DataLayer.Types
import qualified DataLayer.MySql as MySql

data DBFetcher = DBFetcher {
   getByColumn :: GetByColumnTemplate -> [BS.ByteString] -> IO (Either String [ResultRow]),
   -- ^ Fetches all rows matching the given values using the given query
   escapeString :: BS.ByteString -> IO BS.ByteString,
   -- ^ Escapes the given set of characters to make it safe to put in quotes
   getSchema :: IO (Either String [TableSchema]),
   -- ^ Gets a complete schema for the database
   testConnection :: IO Bool
   -- ^ Returns true if the connection is working, false otherwise
}

instance Show DBFetcher where
   show a = "DBFetcher {}"

type Config = [(String, String)]
buildFetcher :: String -> Config -> IO (Either String DBFetcher)
buildFetcher "mysql" configPairs =
   case config of
        Right connInfo -> do
           eitherConnection <- MySql.makeConnection connInfo
           case eitherConnection of
                Right connection -> return . Right $ DBFetcher {
                   getByColumn = MySql.colQuery connection,
                   getSchema = MySql.getSchema connection,
                   escapeString = MySql.escapeString connection,
                   testConnection = MySql.testConnection connection
                }
                Left str -> return $ Left str
        Left str -> return $ Left str
   where
      config = MySql.buildConnInfo configPairs
getFetcher dbType _ = return . Left $ "Can't handle db type: " ++ dbType
