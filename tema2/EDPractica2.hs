-- <pre><div class="text_to_html">-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 2
--
-- Alumno: FERNÁNDEZ MÁRQUEZ, JOSÉ LUIS
-------------------------------------------------------------------------------

import           Data.List
import           Test.QuickCheck

-------------------------------------------------------------------------------
-- X Ejercicio 3 - reparte
-------------------------------------------------------------------------------

reparte :: [a] -> ([a], [a])
reparte [] = ([],[])
reparte x = reparte' x (tail x) ([],[])
    where
      reparte' x y (a,b)
    --      | null x && null y = (a++[],b++[])
          | length y == 1 = (a++[head x], b++[head y])
          | null y = reparte' (tail x) [] (a++[head x],b++[])
          | otherwise = reparte' (tail (tail x)) (tail (tail y)) (a++[head x],b++[head y])

-- @@@@@@@@@@@@@@@@@@@@@@@222  ni caso

-- reparte :: [a] -> ([a], [a])
-- reparte [a] = reparte' [a]
--    where reparte' [a] =
--      if head [a] == null then [a]

--        else ([head [a]]++ reparte' drop 2 [a], head (head [a]))


-------------------------------------------------------------------------------
-- X Ejercicio 4 - distintos
-------------------------------------------------------------------------------

distintos :: Eq a => [a] -> Bool
distintos [] = True
distintos [_] = True
distintos (x:y:xs) =
  if length (x:y:xs) == 0 || length (x:y:xs) == 1 then True
    else if elem x (y:xs) == False && distintos (y:xs) == True then True
      else False
-------------------------------------------------------------------------------
-- X Ejercicio 6 - divisores
-------------------------------------------------------------------------------

divideA :: Integer -> Integer -> Bool
x `divideA` y = mod y x == 0

divisores :: Integer -> [Integer]
divisores x = if x == 0 then [0]
                  else divisoresAc x 1 []
    where divisoresAc x n a
              | n == x = a++[n]
              | otherwise = if n `divideA` x then divisoresAc x (n+1) (a ++ [n])
                               else divisoresAc x (n+1) a

divisores' :: Integer -> [Integer]
divisores' x
    | x>=0 = divisores x
    | otherwise = (reverse (map cambiaSigno (divisores (x*(-1))))) ++ divisores (x*(-1))
             where
               cambiaSigno x = x*(-1)
-------------------------------------------------------------------------------
-- X Ejercicio 8 - primos
-------------------------------------------------------------------------------

-- 8.a
esPrimo :: Integer -> Bool
esPrimo x = if divisores x == [1,x] then True
                else False

-- 8.b
primosHasta :: Integer -> [Integer]
primosHasta x = primosHastaAc x 1 []
    where
      primosHastaAc x n a
          | x==n = if esPrimo n then a++[n] else a
          | otherwise = if esPrimo n then primosHastaAc x (n+1) (a ++ [n])
                          else primosHastaAc x (n+1) a

primosHasta2 :: Integer -> [Integer]
primosHasta2 x = [ x | x <- [1..x ], esPrimo x ]

-- 8.c
primosHasta' :: Integer -> [Integer]
primosHasta' x = filter esPrimo [1..x]

-- 8.d
p1_primos :: Integer -> Bool
p1_primos n = primosHasta2 n == primosHasta' n

-------------------------------------------------------------------------------
-- Ejercicio 13 - hoogle
-------------------------------------------------------------------------------

-- Hoogle (https://www.haskell.org/hoogle/) es un buscador para el API de Haskell.
-- Usa Hoogle para encontrar información sobre las funciones 'and', y 'zip'

desconocida :: Ord a => [a] -> Bool
desconocida xs = and [ x <= y | (x, y) <- zip xs (tail xs) ]

-------------------------------------------------------------------------------
-- X Ejercicio 14 - inserta y ordena
-------------------------------------------------------------------------------

-- 14.a - usando takeWhile y dropWhile
inserta :: Integer -> [Integer] -> [Integer]
inserta x xs = inserta' x (takeWhile (<x) xs) (dropWhile (<x) xs)
    where
      inserta' x a b = (a ++ [x] ++ b)

-- 14.b - mediante recursividad
insertaRec :: undefined
insertaRec = undefined

-- 14.c

-- La línea de abajo no compila hasta que completes los apartados
-- anteriores.

-- p1_inserta x xs = desconocida xs ==> desconocida (inserta x xs)

-- 14.e - usando foldr
ordena :: undefined
ordena = undefined

-- 14.f
prop_ordena_OK = undefined

-------------------------------------------------------------------------------
-- X Ejercicio 23 - nub
-------------------------------------------------------------------------------

-- 23.a
nub' :: Eq a => [a] -> [a]
nub' xs = undefined

-- 23.b
p_nub' xs = nub xs == nub' xs

-- 23.c (distintos se define en el ejercicio 2)

p_sinRepes xs = distintos (nub' xs)

{-

Escribe aquí tu razonamiento de por qué p_sinRepes no
es suficiente para comprobar que nub' es correcta:




-}

-- 23.d
todosEn :: Eq a => [a] -> [a] -> Bool
ys `todosEn` xs = all (`elem` xs) ys

p_sinReps' xs = undefined
-- </div></pre>
