module OrderedQueue (
   OrderedQueue,
   empty,
   isEmpty,
   enqueue,
   dequeue,
   takeWhileSame
) where

import qualified Data.Set as S

newtype OrderedQueue a = OrderedQueue (S.Set a) deriving Show

-- | Create a new queue
empty :: OrderedQueue a
empty = OrderedQueue S.empty

-- | Is the queue empty
isEmpty :: OrderedQueue a -> Bool
isEmpty (OrderedQueue set) = S.null set

-- | Add an element to the queue
enqueue :: Ord a => a -> OrderedQueue a -> OrderedQueue a
enqueue item (OrderedQueue set) = OrderedQueue $ S.insert item set

-- | Pop the smallest element off the queue
dequeue :: Ord a => OrderedQueue a -> Maybe (a, OrderedQueue a)
dequeue (OrderedQueue set) = do
   (item, newSet) <- S.minView set 
   return (item, OrderedQueue newSet)

-- | Splits all items matching the first query type from the rest of the set
takeWhileSame :: (Eq b, Ord a) => (a -> b) -> OrderedQueue a -> ([a], OrderedQueue a)
takeWhileSame extract q = case dequeue q of
                           Just (item, rest) -> takeWhileSame' (extract item) q
                           Nothing -> ([], q)
   where
      takeWhileSame' template q = inner template [] $ dequeue q
      inner template acc (Just (item, rest)) =
         case (extract item) == template of
              True -> inner template (item:acc) $ dequeue rest
              False -> (reverse acc, enqueue item rest)
      inner template acc Nothing = (reverse acc, empty)
