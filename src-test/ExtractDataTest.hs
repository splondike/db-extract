module ExtractDataTest where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.String (fromString)
import Data.Maybe (isJust)
import qualified Data.ByteString as BS
import qualified Data.Set as S
import qualified Control.Concurrent.MVar as MV

import Config(ForeignKey(..))
import ExtractData.Types (Query(..))
import DataLayer.Types (GetByColumnTemplate(..), ResultRow(..))
import qualified ExtractData.Program as P
import qualified DataLayer as DL

allTests = testGroup "ExtractData Tests" [
   testCase "Fetches foreign keys" testFetchesForeignKeys,
   testCase "Terminates when there are FK loops" testHandlesFKLoops,
   testCase "Fetches references from other tables" testFetchesReverseForeignKeys,
   testCase "Handles null foreign keys" testHandlesNullForeignKeys,
   testCase "Handles missing references" testHandlesMissingReferences,
   testCase "Handles multiple query matches" testHandlesMultipleMatches,
   testCase "Batches queries" testBatchesQueries,
   testCase "Filters duplicate queries" testFiltersDuplicateQueries]

testFetchesForeignKeys = checkMatch expectedIO actualIO
   where
      actualIO = P.processQuery dbFetcher initialQuery fks
      expectedIO = return $ S.fromList expectedItems
      expectedItems = [("blogs", ResultRow blogOne), ("authors", ResultRow authorOne)]

      dbFetcher = fetchFromMap tableData
      initialQuery = Query (GetByColumnTemplate "id" "blogs") (fromString "1")
      tableData = [("blogs",   [blogOne, blogTwo]),
                   ("authors", [authorOne])]

testHandlesFKLoops = checkMatch expectedIO actualIO
   where
      actualIO = P.processQuery dbFetcher initialQuery fks
      fks = [ForeignKey "table1" "table2_id" "table2" "id",
             ForeignKey "table2" "table1_id" "table1" "id"]
      expectedIO = return $ S.fromList expectedItems
      expectedItems = [("table1", ResultRow table1Row), ("table2", ResultRow table2Row)]

      dbFetcher = fetchFromMap tableData
      table1Row = toRow [("id", Just "1"), ("table2_id", Just "1"), ("name", Just "bob")]
      table2Row = toRow [("id", Just "1"), ("table1_id", Just "1")]
      initialQuery = Query (GetByColumnTemplate "name" "table1") (fromString "bob")
      tableData = [("table1",   [table1Row]),
                   ("table2", [table2Row])]

testFetchesReverseForeignKeys = checkMatch expectedIO actualIO
   where
      actualIO = P.processQuery dbFetcher initialQuery fks
      expectedIO = return $ S.fromList expectedItems
      expectedItems = [("blogs", ResultRow blogOne), ("authors", ResultRow authorOne)]

      dbFetcher = fetchFromMap tableData
      initialQuery = Query (GetByColumnTemplate "id" "authors") (fromString "1")
      tableData = [("blogs",   [blogOne, blogTwo]),
                   ("authors", [authorOne])]

testHandlesNullForeignKeys = checkMatch expectedIO actualIO
   where
      actualIO = P.processQuery dbFetcher initialQuery fks
      expectedIO = return $ S.fromList expectedItems
      expectedItems = [("blogs", ResultRow blogNoAuthor)]

      dbFetcher = fetchFromMap tableData
      initialQuery = Query (GetByColumnTemplate "id" "blogs") (fromString "3")
      tableData = [("blogs",   [blogOne, blogNoAuthor]),
                   ("authors", [authorOne])]

testHandlesMissingReferences = checkMatch expectedIO actualIO
   where
      actualIO = P.processQuery dbFetcher initialQuery fks
      expectedIO = return $ S.fromList expectedItems
      expectedItems = [("blogs", ResultRow blogOne)]

      dbFetcher = fetchFromMap tableData
      initialQuery = Query (GetByColumnTemplate "id" "blogs") (fromString "1")
      tableData = [("blogs",   [blogOne]),
                   ("authors", [])]

testHandlesMultipleMatches = checkMatch expectedIO actualIO
   where
      actualIO = P.processQuery dbFetcher initialQuery fks
      expectedIO = return $ S.fromList expectedItems
      expectedItems = [("blogs", ResultRow blogOne),
                       ("blogs", ResultRow blogExtra),
                       ("authors", ResultRow authorOne)]

      dbFetcher = fetchFromMap tableData
      initialQuery = Query (GetByColumnTemplate "id" "blogs") (fromString "1")
      tableData = [("blogs",   [blogOne, blogExtra]),
                   ("authors", [authorOne])]

testBatchesQueries = do
   counter <- MV.newMVar 0
   P.processQuery (dbFetcher counter) initialQuery fks
   actualCount <- MV.takeMVar counter
   -- Fetch authors by name, fetch blogs by id, fetch authors by id
   assertEqual "Number of DB calls matches expected" 3 actualCount
   where
      expectedItems = [("blogs", ResultRow blogOne),
                       ("blogs", ResultRow blogExtra),
                       ("authors", ResultRow authorOne)]

      dbFetcher counter = withQueryCount counter dbFetcher'
      dbFetcher' = fetchFromMap tableData
      initialQuery = Query (GetByColumnTemplate "last_name" "authors") (fromString "Smith")
      tableData = [("blogs",   [blogOne, blogTwo]),
                   ("authors", [authorOne, authorTwo])]


testFiltersDuplicateQueries = do
   counter <- MV.newMVar 0
   P.processQuery (dbFetcher counter) initialQuery fks
   actualCount <- MV.takeMVar counter
   assertEqual "Number of DB calls matches expected" 2 actualCount
   where
      expectedItems = [("blogs", ResultRow blogOne),
                       ("blogs", ResultRow blogExtra),
                       ("authors", ResultRow authorOne)]

      dbFetcher counter = withQueryCount counter dbFetcher'
      dbFetcher' = fetchFromMap tableData
      initialQuery = Query (GetByColumnTemplate "author_id" "blogs") (fromString "1")
      tableData = [("blogs",   [blogOne, blogExtra]),
                   ("authors", [authorOne])]

withQueryCount counter dbFetcher' = dbFetcher' {DL.getByColumn = wrapped}
   where
      orig = DL.getByColumn dbFetcher'
      wrapped template ids = do
         rtn <- orig template ids
         MV.modifyMVar_ counter (return . succ)
         return rtn

checkMatch expectedIO actualIO = do
   expected <- expectedIO
   actual <- actualIO
   assertEqual "Actual result did not match expectations" expected actual

fetchFromMap :: [(String, [[(BS.ByteString, Maybe BS.ByteString)]])] -> DL.DBFetcher
fetchFromMap database = DL.DBFetcher{
                           DL.getByColumn = gbc,
                           DL.testConnection = return True,
                           DL.escapeString = return . id,
                           DL.getSchema = return $ Left "Not implemented"
                        }
   where
      gbc template ids = return $ getTableData template database >>=
                                  getRows template ids
      
      getTableData t db = maybeToEither $ lookup (tableName t) db
      getRows t ids rows = return $ map ResultRow $ filter (matchIds t ids) rows
      matchIds _ _ _ = True

      maybeToEither Nothing = Left "Couldn't find table"
      maybeToEither (Just rows) = Right rows

-- Shared Data
blogOne = toRow [("id", Just "1"), ("author_id", Just "1"), ("title", Just "Test")]
blogTwo = toRow [("id", Just "2"), ("author_id", Just "2"), ("title", Just "Another")]
blogNoAuthor = toRow [("id", Just "3"), ("author_id", Nothing), ("title", Just "Missing")]
blogExtra = toRow [("id", Just "3"), ("author_id", Just "1"), ("title", Just "Extra")]
authorOne = toRow [("id", Just "1"), ("first_name", Just "Bob"), ("last_name", Just "Smith")]
authorTwo = toRow [("id", Just "2"), ("first_name", Just "Jane"), ("last_name", Just "Smith")]
fks = [ForeignKey "blogs" "author_id" "authors" "id"]

toRow = map convertPair
convertPair :: (String, Maybe String) -> (BS.ByteString, Maybe BS.ByteString)
convertPair (key, maybeVal) = (fromString key, fmap fromString maybeVal) 
