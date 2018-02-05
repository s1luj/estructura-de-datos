-------------------------------------------------------------------------------
-- Priority Queues with Weight Biased Leftist Heaps
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Heap.WBLHPriorityQueue 
  ( PQueue
  , empty
  , isEmpty
  , minElem
  , delMin
  , enqueue
  ) where

import qualified DataStructures.Heap.WeightBiasedLeftistHeap as H
import Data.List(intercalate)
import Test.QuickCheck

data PQueue a = PQ (H.Heap a) 

empty :: PQueue a
empty  = PQ H.empty

isEmpty :: PQueue a -> Bool
isEmpty (PQ h)  = H.isEmpty h

enqueue :: (Ord a) => a -> PQueue a -> PQueue a
enqueue x (PQ h)  = PQ (H.insert x h)

minElem :: PQueue a -> a
minElem (PQ h)  = H.minElem h

delMin :: (Ord a) => PQueue a -> PQueue a
delMin (PQ h)  = PQ (H.delMin h)

heapToList :: (Ord a) => H.Heap a -> [a]
heapToList h
 | H.isEmpty h = []
 | otherwise   = H.minElem h : heapToList (H.delMin h)

-- Showing a priority queue
instance (Ord a, Show a) => Show (PQueue a) where
  show (PQ h)  = "WBLHPriorityQueue(" ++ intercalate "," (map show . heapToList $ h) ++ ")"

-- priority queue equality
instance (Ord a) => Eq (PQueue a) where
  (PQ h) == (PQ h')  = heapToList h == heapToList h'

-- This instace is used by QuickCheck to generate random stacks
instance (Ord a, Arbitrary a) => Arbitrary (PQueue a) where
    arbitrary =  do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)



