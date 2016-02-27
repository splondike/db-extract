module ExtractData.Types where 

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Ord (Ord, compare, Ordering(EQ, LT, GT))
import qualified Data.ByteString as BS

import DataLayer.Types

data Query = Query GetByColumnTemplate BS.ByteString deriving (Eq, Show)
getTemplate (Query t _) = t
getVal (Query _ v) = v

instance Ord Query where
   compare q1@(Query (GetByColumnTemplate c1 t1) v1) q2@(Query (GetByColumnTemplate c2 t2) v2) = result
      where
         result = fromMaybe EQ $ find firstNonEq comparisons
         firstNonEq EQ = False
         firstNonEq _ = True
         comparisons = [compare t1 t2, compare c1 c2, compare v1 v2]
