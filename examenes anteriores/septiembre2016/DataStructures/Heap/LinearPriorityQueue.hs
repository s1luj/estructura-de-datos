-------------------------------------------------------------------------------
-- Linear implementation of Priority Queues 
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Heap.LinearPriorityQueue 
  ( PQueue
  , empty
  , isEmpty
  , minElem
  , delMin
  , enqueue
  ) where

import Data.List(intercalate)
import Test.QuickCheck

data PQueue a = Empty | Node a (PQueue a) 

empty ::PQueue a
empty  = Empty

isEmpty ::PQueue a -> Bool
isEmpty Empty  = True
isEmpty _      = False

enqueue :: (Ord a) => a ->PQueue a -> PQueue a
enqueue x Empty  = Node x Empty
enqueue x (Node y q) 
 | x <= y        = Node x (Node y q)
 | otherwise     = Node y (enqueue x q)

minElem ::PQueue a -> a
minElem Empty       = error "minElem on empty queue"
minElem (Node x _)  = x

delMin ::PQueue a -> PQueue a
delMin Empty       = error "delMin on empty queue"
delMin (Node _ q)  = q

mkHeap :: (Ord a) => [a] -> PQueue a
mkHeap xs = foldr enqueue empty xs 

-- Showing a priority queue
instance (Show a) => Show (PQueue a) where
  show q = "LinearPriorityQueue(" ++ intercalate "," (aux q) ++ ")"
    where
     aux Empty         =  []
     aux (Node x q) =  show x : aux q

-- priority queue equality
instance (Eq a) => Eq (PQueue a) where
  Empty      == Empty           =  True
  (Node x q) == (Node x' q') =  x==x' && q==q'
  _          == _               =  False

-- This instace is used by QuickCheck to generate random stacks
instance (Ord a, Arbitrary a) => Arbitrary (PQueue a) where
    arbitrary =  do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)



