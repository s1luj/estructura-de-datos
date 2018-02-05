-------------------------------------------------------------------------------
-- Axioms for a Priority Queue
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Heap.PriorityQueueAxioms(ax1,ax2,ax3,ax4,ax5,ax6,ax7,priorityQueueAxioms) where

--import DataStructures.Heap.LinearPriorityQueue
import DataStructures.Heap.WBLHPriorityQueue  
import Test.QuickCheck

ax1        = True    ==>  isEmpty empty
ax2 x q    = True    ==>  not (isEmpty (enqueue x q))
ax3 x y q  = True    ==>  enqueue x (enqueue y q) == enqueue y (enqueue x q)
ax4 x      = True    ==>  minElem (enqueue x empty) == x
ax5 x y q  = x <= y  ==>  minElem (enqueue y (enqueue x q)) == minElem (enqueue x q)
ax6 x      = True    ==>  delMin (enqueue x empty) == empty
ax7 x y q  = x <= y  ==>  delMin (enqueue y (enqueue x q)) == enqueue y (delMin (enqueue x q))

type Elem = Int

priorityQueueAxioms = do
  quickCheck (ax1 :: Property)
  quickCheck (ax2 :: Elem -> PQueue Elem  -> Property)
  quickCheck (ax3 :: Elem -> Elem -> PQueue Elem -> Property)
  quickCheck (ax4 :: Elem ->Property)
  quickCheck (ax5 :: Elem -> Elem -> PQueue Elem -> Property)
  quickCheck (ax6 :: Elem  -> Property)
  quickCheck (ax7 :: Elem -> Elem -> PQueue Elem -> Property)


