module ExtractData.SqlRenderer (
   renderResults
) where

import Data.String(fromString)
import Data.Text (Text(..), unpack)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import qualified Data.ByteString as BS
import qualified Text.Bytedump as BD
import qualified Data.Foldable as F

import DataLayer (DBFetcher(escapeString))
import DataLayer.Types(ResultRow(..))

-- | Converts a collection of (tableName, ResultRow) pairs into a string
-- of SQL insert statements.
--
-- Will silently do the wrong things with tables that contain textual data
-- that isn't UTF-8 compatible.
renderResults :: Foldable t => DBFetcher -> t (String, ResultRow) -> IO Text
renderResults db results = sqlStatementRows >>= (return . decodeUtf8 . joinRows)
   where
      sqlStatementRows = sequence $ F.foldl accumulateRows [] results
      accumulateRows acc row = (renderRow db row):acc

      joinRows rows = BS.intercalate (fromString "\n") rows

renderRow :: DBFetcher -> (String, ResultRow) -> IO BS.ByteString
renderRow db (tableName, tableContents) = do
   (cols, vals) <- quotePairs
   let colStr = BS.intercalate (fromString ",") cols
       valStr = BS.intercalate (fromString ",") vals

   return $ BS.concat [s "INSERT INTO `", s tableName, s "` (", colStr, s ") VALUES (", valStr, s ");"]
   where
      quotePairs = mapM doQuote (getRowData tableContents) >>= (return . unzip)
      doQuote (col, val) = serializeVal val >>= \sv -> return (serializeCol col, sv)

      serializeCol col = (fromString "`") `BS.append` col `BS.append` (fromString "`")
      -- We have to use a different serialization path if the data is binary
      serializeVal (Just val) = escapeString db val >>= \escVal -> case decodeUtf8' escVal of
                                 Left _ -> return $ makeHexString val
                                 Right _ -> return $ quoteVal escVal
      serializeVal Nothing = return $ fromString "NULL"
      quoteVal val = (fromString "'") `BS.append` val `BS.append` (fromString "'")
      makeHexString = (BS.append $ fromString "0x") . fromString . BD.dumpRawBS

      s = fromString
