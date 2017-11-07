-- <pre><div class="text_to_html">-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 2 - Ejercicios extra
--
-- Alumno: FERNÁNDEZ MÁRQUEZ, JOSÉ LUIS
-------------------------------------------------------------------------------

module Practica2Extra where

import           Data.Char
import           Test.QuickCheck
import           Text.Show.Functions

----------------------------------------------------------------------
-- Ejercicio - empareja
----------------------------------------------------------------------

empareja :: [a] -> [b] -> [(a, b)]
empareja xs ys = undefined

prop_empareja_OK :: (Eq b, Eq a) => [a] -> [b] -> Bool
prop_empareja_OK xs ys = undefined

----------------------------------------------------------------------
-- Ejercicio - emparejaCon
----------------------------------------------------------------------

emparejaCon ::  (a -> b -> c) -> [a] -> [b] -> [c]
emparejaCon f xs ys = undefined

prop_emparejaCon_OK :: Eq c => (a -> b -> c) -> [a] -> [b] -> Bool
prop_emparejaCon_OK f xs ys = undefined

----------------------------------------------------------------------
-- Ejercicio - separa
----------------------------------------------------------------------

separaRec :: (a -> Bool) -> [a] -> ([a], [a])
separaRec p xs = undefined

separaC :: (a -> Bool) -> [a] -> ([a], [a])
separaC p xs = undefined

separaP :: (a -> Bool) -> [a] -> ([a], [a])
separaP p xs = undefined

prop_separa_OK :: Eq a => (a -> Bool) -> [a] -> Bool
prop_separa_OK p xs = undefined

----------------------------------------------------------------------
-- Ejercicio - lista de pares
----------------------------------------------------------------------

cotizacion :: [(String, Double)]
cotizacion = [("apple", 116), ("intel", 35), ("google", 824), ("nvidia", 67)]

buscarRec :: Eq a => a -> [(a,b)] -> [b]
buscarRec x ys = undefined

buscarC :: Eq a => a -> [(a, b)] -> [b]
buscarC x ys = undefined

buscarP :: Eq a => a -> [(a, b)] -> [b]
buscarP x ys = undefined

prop_buscar_OK :: (Eq a, Eq b) => a -> [(a, b)] -> Bool
prop_buscar_OK x ys = undefined

{-

Responde las siguientes preguntas si falla la propiedad anterior.

¿Por qué falla la propiedad prop_buscar_OK?

Realiza las modificaciones necesarias para que se verifique la propiedad.

-}

valorCartera :: [(String, Double)] -> [(String, Double)] -> Double
valorCartera cartera mercado = undefined

----------------------------------------------------------------------
-- Ejercicio - mezcla
----------------------------------------------------------------------

-- mezcla :: Ord a => [a] -> [a] -> [a]
-- mezcla [] [] = []
-- mezcla [] y = y
-- mezcla x [] = x
-- mezcla x y = ordenaMezcla x y
--     where
--       ordenaMezcla x y
--         | null x && null y = []
--         | null x = head y ++ (ordenaMezcla [] (tail y))
--         | null y = head x ++ (ordenaMezcla (tail x) [])
--         | head x < head y = (head x) ++ (ordenaMezcla (tail x) y)
--         | otherwise =  head y ++ (ordenaMezcla x (tail y))

-- mezcla[] ys = ys
-- mezcla xs


----------------------------------------------------------------------
-- Ejercicio - takeUntil
----------------------------------------------------------------------

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p xs = undefined

prop_takeUntilOK :: Eq a => (a -> Bool) -> [a] -> Bool
prop_takeUntilOK p xs = undefined

----------------------------------------------------------------------
-- Ejercicio - número feliz
----------------------------------------------------------------------

digitosDe :: Integer -> [Integer]
digitosDe x
    | (div x 10 == 0) = [mod x 10]
    | otherwise = (digitosDe (div x 10)) ++ [mod x 10]

sumaCuadradosDigitos :: Integer -> Integer
sumaCuadradosDigitos x = sumaCuadradosDigitos' (digitosDe x)
    where
      sumaCuadradosDigitos' x
          | null x = 0
          | otherwise = (head x) * (head x) + sumaCuadradosDigitos' (tail x)
-- sumaCuadradosDigitos2 x = foldr (+) 0 (map (^2)(digitosDE(x)))

-- sumaCuadradosDigitos3 x = sum . map (^2) . digitosDe $ x

esFeliz :: Integer -> Bool
esFeliz x = esFeliz' x x
    where
      esFeliz' x y
          | x == 0 = False
--         .................
-- esFeliz2 x = esFelizAc2 x []
--   where
--      esFelizAc2 1 _ = True
--      esFelizAc2 n ac = (n `notElem` ac) && esFelizAc (sumaCuadradosDigitos n) (n:ac)

felicesHasta :: Integer -> [Integer]
felicesHasta x = undefined

{-

Responde a la siguiente pregunta.

¿Cuántos números felices hay menores o iguales que 1000?

-}

----------------------------------------------------------------------
-- Ejercicio - borrar
----------------------------------------------------------------------

borrarRec :: Eq a => a -> [a] -> [a]
borrarRec x ys = undefined

borrarC :: Eq a => a -> [a] -> [a]
borrarC x ys = undefined

borrarP :: Eq a => a -> [a] -> [a]
borrarP x ys = undefined

prop_borrar_OK :: Eq a => a -> [a] -> Bool
prop_borrar_OK x ys = undefined

----------------------------------------------------------------------
-- Ejercicio - agrupar
----------------------------------------------------------------------

agrupar :: Eq a => [a] -> [[a]]
agrupar xs = undefined

----------------------------------------------------------------------
-- Ejercicio - aplica
----------------------------------------------------------------------

aplicaRec :: a -> [ a -> b] -> [b]
aplicaRec x fs = undefined

aplicaC :: a -> [ a -> b] -> [b]
aplicaC x fs = undefined

aplicaP :: a -> [ a -> b] -> [b]
aplicaP x fs = undefined

aplicaM :: a -> [ a -> b] -> [b]
aplicaM x fs = undefined

prop_aplica_OK :: Eq b => a -> [a -> b] -> Bool
prop_aplica_OK x fs = undefined
-- </div></pre>
