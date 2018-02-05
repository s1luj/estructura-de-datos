-- | Data Structures
-- | September, 2016
-- |
-- | Student's name:
-- | Student's group:

module Huffman where

import qualified DataStructures.Dictionary.AVLDictionary as D
import qualified DataStructures.PriorityQueue.WBLeftistHeapPriorityQueue as PQ
import Data.List (nub)

-- | Exercise 1

weights :: Ord a => [a] -> D.Dictionary a Int
weights [] = D.empty 
weights (x:xs) = D.insert x (f xs) (weights (f2 xs))
    where 
        f [] = 1
        f (y:ys) 
                | x == y = 1 + (f ys)  
                | otherwise = (f ys)
        f2 [] = []
        f2 (z:zs) 
                | x == z = (f2 zs)
                | otherwise = [z] ++ (f2 zs)

{-

> weights "abracadabra"
AVLDictionary('a'->5,'b'->2,'c'->1,'d'->1,'r'->2)

> weights [1,2,9,2,0,1,6,1,5,5,8]
AVLDictionary(0->1,1->3,2->2,5->2,6->1,8->1,9->1)

> weights ""
AVLDictionary()

-}


-- Implementation of Huffman Trees
data WLeafTree a = WLeaf a Int  -- Stored value (type a) and weight (type Int)
                 | WNode (WLeafTree a) (WLeafTree a) Int -- Left child, right child and weight
                 deriving (Eq, Show)

weight :: WLeafTree a -> Int
weight (WLeaf _ n)   = n
weight (WNode _ _ n) = n

-- Define order on trees according to their weights
instance Eq a => Ord (WLeafTree a) where
  wlt <= wlt' =  weight wlt <= weight wlt'

-- Build a new tree by joining two existing trees
merge :: WLeafTree a -> WLeafTree a -> WLeafTree a
merge wlt1 wlt2 = WNode wlt1 wlt2 (weight wlt1 + weight wlt2)


-- | Exercise 2

-- 2.a
huffmanLeaves :: String -> PQ.PQueue (WLeafTree Char)
huffmanLeaves [] = PQ.empty
huffmanLeaves (x:xs) = ordenar (x:xs) [x] 
--PQ.enqueue (WLeaf x (peso(D.valueOf(x) dic))) (huffmanLeaves (f xs))
    where
        dic = (weights (x:xs)) 
        peso (Just a) = a 
        peso Nothing = 0
        f [] = []
        f (z:zs) 
                | x == z = (f zs)
                | otherwise = [z] ++ (f zs)
        ordenar [] [] = huffmanLeaves []
        ordenar [] (b:bs) = PQ.enqueue (WLeaf b (peso(D.valueOf(b) dic))) (ordenar [] bs)
        ordenar (a:aa) [] = ordenar aa [a]
        ordenar (a:aa) (b:bs)
                | contiene a (b:bs) = ordenar aa (b:bs)
                | (peso(D.valueOf(a) dic)) < (peso(D.valueOf(b) dic)) = ordenar aa ([a] ++ (b:bs))
                | (peso(D.valueOf(a) dic)) == (peso(D.valueOf(b) dic)) = if a<b then ordenar aa ([a] ++ (b:bs)) else ordenar aa ([b] ++ (a:bs))
                | otherwise = ordenar aa (ordenar2 a (b:bs))
        ordenar2 e [] = [e]
        ordenar2 e (c:cs)
                | (peso(D.valueOf(e) dic)) < (peso(D.valueOf(c) dic)) = [e] ++ cs 
                | otherwise = [c] ++ ordenar2 e cs
        contiene rr [] = False
        contiene rr (r:rs)
                | rr == r = True 
                | otherwise = contiene rr rs
            
       

{-

> huffmanLeaves "abracadabra"
WBLeftistHeapPriorityQueue(WLeaf 'c' 1,WLeaf 'd' 1,WLeaf 'b' 2,WLeaf 'r' 2,WLeaf 'a' 5)

-}

-- 2.b
huffmanTree :: String -> WLeafTree Char
huffmanTree xs = f arbol (PQ.dequeue (PQ.dequeue hojas)) 
  where 
    hojas = (huffmanLeaves xs)
    arbol = WNode (PQ.first hojas)  (PQ.first (PQ.dequeue hojas)) ((peso (PQ.first hojas)) + (peso(PQ.first (PQ.dequeue hojas)))) 
    peso (WLeaf _ n) = n
    peso2 (WNode _ _ m) = m
    dic = (weights xs)
    f arbol hojas 
        | (PQ.isEmpty hojas) == True = arbol
        | (peso2 arbol) > (peso (PQ.first hojas)) = f (WNode (PQ.first hojas) arbol ((peso2 arbol) + (peso (PQ.first hojas))))   (PQ.dequeue hojas)
        | otherwise = f (WNode arbol (PQ.first hojas) ((peso2 arbol) + (peso (PQ.first hojas))))   (PQ.dequeue hojas)
        
           

{-

> printWLeafTree $ huffmanTree "abracadabra"
      11_______
      /        \
('a',5)        6_______________
              /                \
        ('r',2)          _______4
                        /       \
                       2        ('b',2)
                      / \
                ('c',1) ('d',1)

> printWLeafTree $ huffmanTree "abracadabra pata de cabra"
                         ______________________25_____________
                        /                                     \
         ______________10______                          ______15
        /                      \                        /       \
       4_______                6_______                6        ('a',9)
      /        \              /        \              / \
('d',2)        2        (' ',3)        3        ('b',3) ('r',3)
              / \                     / \
        ('e',1) ('p',1)         ('t',1) ('c',2)

> printWLeafTree $ huffmanTree "aaa"
*** Exception: huffmanTree: the string must have at least two different symbols

-}


-- | Exercise 3

-- 3.a
joinDics :: Ord a => Ord b => D.Dictionary a b -> D.Dictionary a b -> D.Dictionary a b
joinDics d1 d2 = f (D.keys d1) (D.keys d2)
    where 
     f [] []= D.empty
     f (x:xs) [] = D.insert x (p(D.valueOf(x) d1)) (f xs [])
     f [] (y:ys) = D.insert y (p(D.valueOf(y) d2)) (f [] ys) 
     f (x:xs) (y:ys) 
            | (p(D.valueOf(x) d1)) < (p(D.valueOf(y) d2)) = D.insert x (p(D.valueOf(x) d1)) (f xs (y:ys))
            | (p(D.valueOf(x) d1)) == (p(D.valueOf(y) d2)) = if x<y then D.insert x (p(D.valueOf(x) d1)) (f xs (y:ys)) else D.insert y (p(D.valueOf(y) d2)) (f (x:xs) ys)
            | otherwise = D.insert y (p(D.valueOf(y) d2)) (f (x:xs) ys)  
     p (Just a) = a 
     
{-

> joinDics (D.insert 'a' 1 $ D.insert 'c' 3 $ D.empty) D.empty
AVLDictionary('a'->1,'c'->3)

> joinDics (D.insert 'a' 1 $ D.insert 'c' 3 $ D.empty) (D.insert 'b' 2 $ D.insert 'd' 4 $ D.insert 'e' 5 $ D.empty)
AVLDictionary('a'->1,'b'->2,'c'->3,'d'->4,'e'->5)

-}

-- 3.b
prefixWith :: Ord a => b -> D.Dictionary a [b] -> D.Dictionary a [b]
prefixWith x dic = f x (D.keys dic)
    where 
     f x [] = D.empty
     f x (y:ys) = D.insert y ([x] ++ p(D.valueOf(y) dic)) (f x ys)
     p (Just a) = a
        
        

{-

> prefixWith 0 (D.insert 'a' [0,0,1] $ D.insert 'b' [1,0,0] $ D.empty)
AVLDictionary('a'->[0,0,0,1],'b'->[0,1,0,0])

> prefixWith 'h' (D.insert 1 "asta" $ D.insert 2 "echo" $ D.empty)
AVLDictionary(1->"hasta",2->"hecho")

-}

-- 3.c
{-data WLeafTree a = WLeaf a Int  -- Stored value (type a) and weight (type Int)
                 | WNode (WLeafTree a) (WLeafTree a) Int -}
huffmanCode :: WLeafTree Char -> D.Dictionary Char [Integer]
huffmanCode (WNode a b _) =  combine (D.keys (prefixWith 0 (f a))) (prefixWith 0 (f a)) (prefixWith 1 (f b))
    where 
        combine [] dic1 dic2 = dic2
        combine (x:xs) dic1 dic2 = D.insert x (v(D.valueOf(x) dic1)) (combine xs dic1 dic2)
        v (Just a) = a
        f (WLeaf x _) = D.insert x [] D.empty
        f (WNode (WLeaf x _) (WLeaf y _) _) = D.insert x [0] (D.insert y [1] (D.empty)) 
        f (WNode x (WLeaf y _) _) = D.insert y [1] (prefixWith 0 (f x))
        f (WNode (WLeaf x _) y _) = D.insert x [0] (prefixWith 1 (f y))
        f arbol@(WNode x y _) = huffmanCode arbol

{-

> huffmanCode (huffmanTree "abracadabra")
AVLDictionary('a'->[0],'b'->[1,1,1],'c'->[1,1,0,0],'d'->[1,1,0,1],'r'->[1,0])

-}

-- ONLY for students not taking continuous assessment

-- | Exercise 4

encode :: String -> D.Dictionary Char [Integer] -> [Integer]
encode = undefined

{-

> encode "abracadabra" (huffmanCode (huffmanTree "abracadabra"))
[0,1,1,1,1,0,0,1,1,0,0,0,1,1,0,1,0,1,1,1,1,0,0]

-}

-- | Exercise 5

-- 5.a
takeSymbol :: [Integer] -> WLeafTree Char -> (Char, [Integer])
takeSymbol = undefined

{-

> takeSymbol [0,1,1,1,1,0,0,1,1,0,0,0,1,1,0,1,0,1,1,1,1,0,0] (huffmanTree "abracadabra")
('a',[1,1,1,1,0,0,1,1,0,0,0,1,1,0,1,0,1,1,1,1,0,0])

> takeSymbol [1,1,1,1,0,0,1,1,0,0,0,1,1,0,1,0,1,1,1,1,0,0] (huffmanTree "abracadabra")
('b',[1,0,0,1,1,0,0,0,1,1,0,1,0,1,1,1,1,0,0])

-}

-- 5.b
decode :: [Integer] -> WLeafTree Char -> String
decode = undefined

{-

> decode [0,1,1,1,1,0,0,1,1,0,0,0,1,1,0,1,0,1,1,1,1,0,0] (huffmanTree "abracadabra")
"abracadabra"

-}

-------------------------------------------------------------------------------
-- Pretty Printing a WLeafTree
-- (adapted from http://stackoverflow.com/questions/1733311/pretty-print-a-tree)
-------------------------------------------------------------------------------

printWLeafTree :: (Show a) => WLeafTree a -> IO ()
printWLeafTree t = putStrLn (unlines xss)
 where
   (xss,_,_,_) = pprint t

pprint :: Show a => WLeafTree a -> ([String], Int, Int, Int)
pprint (WLeaf x we)             =  ([s], ls, 0, ls-1)
  where
    s = show (x,we)
    ls = length s
pprint (WNode lt rt we)         =  (resultLines, w, lw'-swl, totLW+1+swr)
  where
    nSpaces n = replicate n ' '
    nBars n = replicate n '_'
    -- compute info for string of this node's data
    s = show we
    sw = length s
    swl = div sw 2
    swr = div (sw-1) 2
    (lp,lw,_,lc) = pprint lt
    (rp,rw,rc,_) = pprint rt
    -- recurse
    (lw',lb) = if lw==0 then (1," ") else (lw,"/")
    (rw',rb) = if rw==0 then (1," ") else (rw,"\\")
    -- compute full width of this tree
    totLW = maximum [lw', swl,  1]
    totRW = maximum [rw', swr, 1]
    w = totLW + 1 + totRW
{-
A suggestive example:
     dddd | d | dddd__
        / |   |       \
      lll |   |       rr
          |   |      ...
          |   | rrrrrrrrrrr
     ----       ----           swl, swr (left/right string width (of this node) before any padding)
      ---       -----------    lw, rw   (left/right width (of subtree) before any padding)
     ----                      totLW
                -----------    totRW
     ----   -   -----------    w (total width)
-}
    -- get right column info that accounts for left side
    rc2 = totLW + 1 + rc
    -- make left and right tree same height
    llp = length lp
    lrp = length rp
    lp' = if llp < lrp then lp ++ replicate (lrp - llp) "" else lp
    rp' = if lrp < llp then rp ++ replicate (llp - lrp) "" else rp
    -- widen left and right trees if necessary (in case parent node is wider, and also to fix the 'added height')
    lp'' = map (\s -> if length s < totLW then nSpaces (totLW - length s) ++ s else s) lp'
    rp'' = map (\s -> if length s < totRW then s ++ nSpaces (totRW - length s) else s) rp'
    -- first part of line1
    line1 = if swl < lw' - lc - 1 then
                nSpaces (lc + 1) ++ nBars (lw' - lc - swl) ++ s
            else
                nSpaces (totLW - swl) ++ s
    -- line1 right bars
    lline1 = length line1
    line1' = if rc2 > lline1 then
                line1 ++ nBars (rc2 - lline1)
             else
                line1
    -- line1 right padding
    line1'' = line1' ++ nSpaces (w - length line1')
    -- first part of line2
    line2 = nSpaces (totLW - lw' + lc) ++ lb
    -- pad rest of left half
    line2' = line2 ++ nSpaces (totLW - length line2)
    -- add right content
    line2'' = line2' ++ " " ++ nSpaces rc ++ rb
    -- add right padding
    line2''' = line2'' ++ nSpaces (w - length line2'')
    resultLines = line1'' : line2''' : zipWith (\lt rt -> lt ++ " " ++ rt) lp'' rp''