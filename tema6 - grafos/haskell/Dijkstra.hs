-- <pre><div class="text_to_html">-------------------------------------------------------------------------------
-- Estructuras de Datos. Grado en Informática, IS e IC. UMA.
-- Ejercicios Práctica 7. Algoritmo de Dijkstra
--
-- Alumno:
-- Titulación:
-------------------------------------------------------------------------------

module Dijkstra where

import           Data.List                               (delete, minimumBy)
import qualified DataStructures.Dictionary.AVLDictionary as D
import           DataStructures.Graph.WeightedGraph

-- dijkstra g  v :  aplica el  algoritmo de Dijkstra  al grafo  g para
-- calcular los caminos mínimos desde el nodo v

dijkstra :: Ord a => WeightedGraph a Int -> a -> D.Dictionary a (Int, Path a)
dijkstra g v
   | v `elem` vs = dijkstraAux g [v] (delete v vs) (D.insert v (0,[v]) D.empty)
   | otherwise   = error "el vértice v no está en el grafo g"
   where
      vs = vertices g

-- dijkstraAux g vs rs d : vs  son los nodos visitados para los que ya
-- se conoce el  camino mínimo, rs el  resto de nodos por  visitar y d
-- almacena los caminos mínimos calculados hasta el momento

dijkstraAux :: Ord a => WeightedGraph a Int -> [a] -> [a] -> D.Dictionary a (Int, Path a) -> D.Dictionary a (Int, Path a)
dijkstraAux g vs [] d = d
dijkstraAux g vs rs d = dijkstraAux g (v':vs) (delete v' rs) (D.insert v' (p',cs') d) -- llamada recursiva
   where
      -- obtener los caminos del diccionario
      caminos = D.keysValues d -- caminos => Iterador (keys, values, keysValues)
      -- extender los caminos del diccionario con aristas hasta nodo en rs
      extensiones = [(r, w+w', cs ++ [r]) | (v, (w,cs) ) <- caminos, (r,w') <- successors g v, elem r rs] -- comprension
      --- seleccionar la extensión de menor coste
      (v', p', cs') = minimumBy comparaCaminos extensiones -- minimunBy
      -- comparar caminos por peso
      comparaCaminos (_, p1, _) (_, p2, _) = compare p1 p2 -- compara por pesos

shortestPaths :: Ord a => WeightedGraph a Int -> a -> [Path a]
shortestPaths g v = undefined

-- ejemplos de grafos

g1 :: WeightedGraph Char Int
g1 = mkWeightedGraphEdges ['a','b','c','d','e']
                          [ WE 'a' 3 'b', WE 'a' 7 'd'
                          , WE 'b' 4 'c', WE 'b' 2 'd'
                          , WE 'c' 5 'd', WE 'c' 6 'e'
                          , WE 'd' 4 'e'
                          ]

-- |
-- >>> dijkstra g1 'a'
-- AVLDictionary('a'->(0,"a"),'b'->(3,"ab"),'c'->(7,"abc"),'d'->(5,"abd"),'e'->(9,"abde"))
--
-- >>> shortestPaths g1 'a'
-- ["a","ab","abc","abd","abde"]

gEjer3 :: WeightedGraph Char Int
gEjer3 = mkWeightedGraphEdges ['a','b','c','d','e','f','g']
                              [ WE 'a' 7 'b', WE 'a' 5 'd'
                              , WE 'b' 9 'd', WE 'b' 8 'c', WE 'b' 7 'e'
                              , WE 'c' 5 'e'
                              , WE 'd' 15 'e', WE 'd' 6 'f'
                              , WE 'e' 8 'f', WE 'e' 9 'g'
                              , WE 'f' 11 'g'
                              ]

-- |
-- >>> dijkstra gEjer3 'a'
-- AVLDictionary('a'->(0,"a"),'b'->(7,"ab"),'c'->(15,"abc"),'d'->(5,"ad"),'e'->(14,"abe"),'f'->(11,"adf"),'g'->(22,"adfg"))
--
-- >>> shortestPaths gEjer3 'a'
-- ["a","ab","abc","ad","abe","adf","adfg"]
-- </div></pre>
