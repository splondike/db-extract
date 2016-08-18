module ExtractData.Program (
   processQuery
) where

import Data.Maybe (isJust)
import Data.String (fromString)
import qualified OrderedQueue as OQ
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString as BS

import qualified DataLayer as DL
import Config (ForeignKey(..))
import DataLayer.Types(GetByColumnTemplate(..), ResultRow(..))
import ExtractData.Types (Query(..), getTemplate, getVal)

processQuery :: DL.DBFetcher -> Query -> [ForeignKey] -> IO (S.Set (String, ResultRow))
processQuery db initialQuery foreignKeys = resultMap >>= (return . makeResults)
   where
      resultMap = inner M.empty (OQ.enqueue initialQuery OQ.empty)

      makeResults m = M.foldrWithKey addToSet S.empty m
      addToSet q v set = foldr (\r s -> S.insert (table q, r) s) set v
      table q = tableName $ getTemplate q

      inner results queries
         | OQ.isEmpty queries = return results
         | otherwise = do
            let (unfetchedQueries, remainingQueries) = extractNextBatch results queries

            fetchedResults <- performQueries db unfetchedQueries

            let combinedResults = M.union results fetchedResults 
            let newQueries = findQueries foreignKeys fetchedResults
                combinedQueries = foldl (flip OQ.enqueue) remainingQueries newQueries

            inner combinedResults combinedQueries

extractNextBatch :: M.Map Query [ResultRow] -> OQ.OrderedQueue Query -> ([Query], OQ.OrderedQueue Query)
extractNextBatch alreadyFetched nextQueries = (unfetchedQueries, remainingQueries)
   where
      unfetchedQueries = filter (flip M.notMember $ alreadyFetched) queriesForTable
      (queriesForTable, remainingQueries) = OQ.takeWhileSame getTemplate nextQueries

performQueries :: DL.DBFetcher -> [Query] -> IO (M.Map Query [ResultRow])
performQueries _ [] = return M.empty
performQueries db qs = do
   let template = getTemplate $ head qs
       vals = map getVal qs
   maybeResults <- DL.getByColumn db template vals
   case maybeResults of
      Left err -> error err
      Right rows -> return $ buildMap qs rows

buildMap :: [Query] -> [ResultRow] -> M.Map Query [ResultRow]
buildMap qs resultRows = foldl addPair M.empty qs
   where
      addPair currMap q = case filter (matches q) resultRows of
                                 [] -> currMap
                                 matches -> M.insert q matches currMap
      matches q row = isJust $ do
         let colName = getColumnName q
             pairs = getRowData row
         val <- lookup colName pairs
         case val == (Just $ getVal q) of
              True -> Just ()
              False -> Nothing

-- TODO: Pick a better name or write some doco
findQueries :: [ForeignKey] -> M.Map Query [ResultRow] -> [Query]
findQueries foreignKeys resultsMap = concat $ M.foldMapWithKey inner resultsMap
   where
      inner query rows = map (\row -> foldl (findMatches row) [] newQueries) rows
         where
            findMatches row c (fromCol, template) = case lookup fromCol (getRowData row) of
                                                      Just (Just val) -> (Query template val):c
                                                      Just Nothing -> c
                                                      Nothing -> c
            newQueries = colQueries $ tableName $ getTemplate query

      colQueries table = fromPairs ++ toPairs
         where
            fromPairs = filterBuild $ map getFromData foreignKeys
            toPairs = filterBuild $ map getToData $ filter unidirectional foreignKeys
            filterBuild xs = map build $ filter (tableMatches table) xs

      getFromData (ForeignKey ft fc tt tc _) = (ft, fc, tt, tc)
      getToData (ForeignKey ft fc tt tc _) = (tt, tc, ft, fc)
      build (ft, fc, tt, tc) = (fromString fc, GetByColumnTemplate{columnName=tc, tableName=tt})
      tableMatches table (a,_,_,_) = a==table
      unidirectional (ForeignKey _ _ _ _ b) = not b

getColumnName :: Query -> BS.ByteString
getColumnName q = fromString $ columnName $ getTemplate q
