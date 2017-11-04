-- <pre><div class="text_to_html">-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 1 - Ejercicios extra
--
-- Alumno: FERNÁNDEZ MÁRQUEZ, JOSÉ LUIS
-------------------------------------------------------------------------------

module Practica1Extra where

import           Test.QuickCheck

----------------------------------------------------------------------
-- Ejercicio - esPrimo
----------------------------------------------------------------------

-- esPrimo :: completa la declaración de tipo
--esPrimo :: Num a => a -> Bool
esPrimo n
 | n<=0 = error "esPrimo: argumento negativo o cero"
 | otherwise = n >=2 && esPrimo' 2
 where
   esPrimo' d
       | n ==d =True
       | otherwise = n `mod` d/=0 && esPrimo' (d+1)



----------------------------------------------------------------------
-- Ejercicio - cocienteYResto
----------------------------------------------------------------------

-- cocienteYResto :: completa la declaración de tipo
--cocienteYResto ::  Num a => a -> a -> (a,a)
cocienteYResto x y
 | y == 0 = error "ERROR: No se puede dividir por 0"
 | x<0 || y<0 = error "ERROR: Argumentos negativos"
 | otherwise = cocienteYResto' x y 0
 where
   cocienteYResto' x y c =
        if x>=y then cocienteYResto' (x-y) y (c+1)
          else (c,x)

prop_cocienteYResto_OK x y
    | y==0 = True
    | x<0 || y<0 = True
    | otherwise = (div x y, mod x y) == cocienteYResto x y

-- Ejercicio - libre de cuadrados
----------------------------------------------------------------------
-- @@@@@@#####
libreDeCuadrados :: Integer -> Bool
libreDeCuadrados n
    | n<=0 = error "ERROR: argumento cero o negativo"
    | esPrimo n = True
    | otherwise = libreDeCuadrados (n-1)


----------------------------------------------------------------------
-- Ejercicio - raiz entera
----------------------------------------------------------------------

raizEntera :: Integer -> Integer
raizEntera x
    | x < 0 = error "argumento negativo"
    | otherwise = raizEntera' x 0
        where
          raizEntera' x y =
            if y*y>x then (y-1)
             else raizEntera' x (y+1)
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@
raizEnteraRapida :: Integer -> Integer
raizEnteraRapida n =
  if n<0 then error "argumento negativo"
    else raizEnteraRapida' 0 n n (-1)
     where
       raizEnteraRapida' inf sup n r=
         if (cuadradoMitad inf sup)==n then mitad inf sup
      --     else if r/=(-1) && r<n then r
            else if (cuadradoMitad inf sup)>n then raizEnteraRapida' inf (mitad inf sup) n r
              else raizEnteraRapida' (mitad inf sup) sup n (mitad inf sup)

       cuadradoMitad inf sup = (mitad inf sup) * (mitad inf sup)

       mitad inf sup = (div (inf+sup) 2)


prop_raizEntera_OK n =
  n>=0 ==> truncate (sqrt (fromIntegral n)) == raizEntera n

----------------------------------------------------------------------
-- Ejercicio - números de Harshad
----------------------------------------------------------------------

sumaDigitos :: Integer -> Integer
sumaDigitos n
  | n < 0 = error "argumento negativo"
  | otherwise =  sumaDigitos' (div n 10) (mod n 10)
      where
        sumaDigitos' c r
          | c == 0 = r
          | otherwise = r + sumaDigitos' (div c 10) (mod c 10)

harshad :: Integer -> Bool
harshad x =
  if x<=0 then error "entero no positivo o igual a cero"
   else if mod x (sumaDigitos x) == 0 then True
    else False

harshadMultiple :: Integer -> Bool
harshadMultiple n = undefined

vecesHarshad :: Integer -> Integer
vecesHarshad n = undefined

prop_Bloem_Harshad_OK :: Integer -> Property
prop_Bloem_Harshad_OK n = undefined

----------------------------------------------------------------------
-- Ejercicio - ceros del factorial
----------------------------------------------------------------------

factorial :: Integer -> Integer
factorial n  = undefined

cerosDe :: Integer -> Integer
cerosDe n = undefined

prop_cerosDe_OK :: Integer -> Integer -> Property
prop_cerosDe_OK n m = undefined

{-

Responde las siguientes preguntas:

¿En cuántos ceros acaba el factorial de 10?

¿En cuántos ceros acaba el factorial de 100?

¿En cuántos ceros acaba el factorial de 1000?

¿En cuántos ceros acaba el factorial de 10000?


-}

----------------------------------------------------------------------
-- Ejercicio - números de Fibonacci y fórmula de Binet
----------------------------------------------------------------------

fib :: Integer -> Integer
fib n  = undefined

llamadasFib :: Integer -> Integer
llamadasFib n = undefined

{-

Responde a las siguientes preguntas:

¿Cuántas llamadas a fib son necesarias para calcular fib 30?


¿Cuántas llamadas a fib son necesarias para calcular fib 60?


-}

fib' :: Integer -> Integer
fib' n = undefined

prop_fib_OK :: Integer -> Property
prop_fib_OK n = undefined

phi :: Double
phi = undefined

binet :: Integer -> Integer
binet n = undefined

prop_fib'_binet_OK :: Integer -> Property
prop_fib'_binet_OK n = undefined

{-

Responde a la siguiente pregunta:

¿A partir de qué valor devuelve binet resultados incorrectos?

-}
-- </div></pre>
