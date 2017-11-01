-------------------------------------------------------------------
-- Estructuras de Datos
-- Grado en Ingeniería Informática, del Software y de Computadores
-- Tema 1. Introducción a la Programación Funcional
-- Pablo López
-------------------------------------------------------------------

module Tema1 where

import           Data.Char

-- Haskell = Puro + Tipificado estático fuerte + Perezoso

-- Puro: los datos son inmutables (como las String de Java)

-- Tipificado estático fuerte:
--    el tipo se establece en tiempo de compilación y no varía
--    no se pueden mezclar tipos distintos
--    no hay conversión implícita entre tipos (ni siquiera números)

-- Perezoso: sólo se evalúa lo necesario

-- ejemplos de pereza:

-- la función undefined provoca un error, pero en los siguientes ejemplos
-- no siempre es necesario evaluarla:

lista = [1,2,3,4]
listaUndef = [1,2,undefined,4] -- error
sumaLista = sum [1,2,3,4]
sumaUndef = sum [1,2,undefined,4] -- error
longitudLista = length [1,2,3,4]
longitudUndef = length [1,2,undefined,4] -- funciona a pesar de undefined

-- pereza y cortocircuito
noFalla1 = False && (1 == 7 `div` 0) -- el segundo operando no se evalúa
noFalla2 = fst (3 + 4, 7 `div` 0) -- la segunda componente no se evalúa

síFalla1 = True  && (1 == 7 `div` 0) -- el segundo operando sí se evalúa
síFalla2 = snd (3 + 4, 7 `div` 0) -- la segunda componente sí se evalúa

{-

Ejercicios de currificación

    f(x+y)

    f(x+1,y)

    f(2*x,y+1)

    g(f(x,y))

    g(x)+y

    f(x,y) + u*v

    max(max(x,y+1), max(z,-z))
-}

{-
Ejercicios de descurrificación

    f 2 + 3 - g x y + 7

    g (2+x) y * 6

    f g x

    f (g x)

    f f f x

    max (abs (-7)) 6*x
-}

-- Definición de funciones en Haskell

-- twice

{-

   int twice(int x) {
      return x + x;
   }

-}

twice :: Integer -> Integer -- declaración
twice x = x + x             -- definición

-- second

{-

   int second(int x, int y) {
      return y
   }

-}

second :: Integer -> Integer -> Integer
second x y = y

-- square

{-

   int square(int x) {
      return x*x;
   }

-}

square :: Integer -> Integer
square x = x * x

-- pythagoras

{-

   int pythagoras(int x, int y) {
      return square(x) + square(y);
   }

-}

pythagoras :: Integer -> Integer -> Integer
pythagoras x y = square x + square y

-- el condicional if then else en Haskell
-- es una expresión, no una sentencia;
-- el else es obligatorio

-- máximo (predefinida como max)
máximo :: Integer -> Integer -> Integer
máximo x y = if x >= y then x else y

máximoDeTres :: Int -> Int -> Int -> Int
máximoDeTres x y z =
  if x >= y && x >= z then x
  else if y >= z then y
  else z

-- recursión: el factorial
factorial :: Integer -> Integer
factorial x = if x == 0 then 1
              else x * factorial (x-1)

-- signo (predefinida como signum)

{-

   int signo(int x) {
      if (x == 0) return  0;
      if (x <  0) return -1;
      if (x >  0) return  1;
   }

-}

signo :: Integer -> Integer
signo x = if x == 0 then 0
          else if x < 0 then -1
          else 1

-- a partir de 2 casos es mejor emplear
-- guardas que anidar if then else

máximoDeTres' :: Int -> Int -> Int -> Int
máximoDeTres' x y z
  | x >= y && x >= z = x
  | y >= z = y
  | otherwise = z


signo' :: Integer -> Integer
signo' x
  | x == 0 = 0
  | x < 0 = -1
  | otherwise = 1

-- operadores

-- una función binaria (div, max, ...) se puede usar como operador
--
--   div x y == x `div` y
--   max x y == x `max` y

-- función max usada como operador: `max`
máximoDeTres'' :: Int -> Int -> Int -> Int
máximoDeTres'' x y z = x `max` y `max` z

-- órdenes de reducción

-- tuplas

-- el número de componentes fijo
-- cada componente puede ser de un tipo distinto
-- es un producto cartesiano
-- existe la tupla vacía: ()
-- no existe la tupla unitaria: (x)
-- utilidad de las tuplas: una función puede devolver varios datos

sucPred :: Integer -> (Integer, Integer)
sucPred x = (x+1, x-1)

códigosDe :: Char -> (Int, Int)
códigosDe x = (ord (toLower x), ord (toUpper x))

{-

Ejercicios de tipado de tuplas:

Suponiendo que las expresiones enteras son de tipo Integer y las
expresiones reales son de tipo Double, tipar las siguientes
expresiones:


(2 `div` 3, True, 'z')

( 1 < 2, if 5 < 7 then 'a' else 'b', -1, (7, 'c'))

(8, (), toLower 'A')

(8, ('a'), toLower 'A')

(8, (()), toLower 'A')

( (ord 'a', ord 'z') , (ord 'A', ord 'Z') )

-}

-- versiones monomórficas (tipos concretos)

primeroI :: (Integer, Integer) -> Integer
primeroI (x,y) = x

primeroC :: (Char, Char) -> Char
primeroC (x,y) = x

primeroB :: (Bool, Bool) -> Bool
primeroB (x,y) = x

-- Versiones polimórficas (variables de tipo)

-- lo que Java llama genericidad en Haskell se llama polimorfismo
-- si un tipo empieza por minúscula es una variable de tipo
-- en Java se escribe <T>, en Haskell se escribe a
-- suelen utilizarse las primeras letras del alfabeto: a,b,c
-- ver ejemplo en Java (Primero.java)

-- versiones polimórficas de las funciones anteriores
-- el código es el mismo, sólo cambia el tipo

-- predefinida como fst
primero :: (a,b) -> a
primero (x,y) = x

-- predefinida como snd
segundo :: (a,b) -> b
segundo (x,y) = y

-- Polimorfismo restringido: el sistema de clases

-- en Java el genérico <T> puede ser reemplazado por cualquier tipo (clase)
-- en Haskell una variable de tipo a puede ser reemplazada por cualquier tipo
-- en Java puedo restringir un genérico: T extends Comparable<T>
-- en Haskell puedo restringir una variable de tipo: Ord a => a
-- una clase Haskell es semejante (pero no equivalente) a una interfaz Java
-- ver ejemplo en Java (Maximo.java)

-- la clase Eq

-- mismo tipo
sonSimétricos :: Eq a => (a,a) -> (a,a) -> Bool
sonSimétricos (x,y) (u,v) = x == v && y == u

-- tipos distintos
sonSimétricos' :: (Eq a, Eq b)  => (a,b) -> (b,a) -> Bool
sonSimétricos' (x,y) (u,v) = x == v && y == u

-- la clase Ord
estáOrdenada :: Ord a => (a,a) -> Bool
estáOrdenada (x,y) = x <= y

-- la clase Num
cubo :: Num a => a -> a
cubo x = x * x * x

-- la clase Integral: división entera
esMúltiplo :: Integral a => a -> a -> Bool
esMúltiplo nx x = nx `mod` x == 0

-- la clase Fractional: división con decimales
inversoNoNulo :: Fractional a => a -> a
inversoNoNulo x = 1 / x

-- la función error es como un throw de Java simplificado
inverso :: Double -> Double
inverso x
   | x == 0 = error "inverso: división por cero"
   | otherwise = 1 / x

-- Definiciones locales con where

raíces :: Double -> Double -> Double -> (Double, Double)
raíces a b c = ((-b + sqrt (b*b-4*a*c)) / (2*a),
                (-b - sqrt (b*b-4*a*c)) / (2*a))

-- mejora la modularidad, la legibilidad y la eficiencia

-- raíces 2 7 1 - dos reales
-- raíces 2 4 2 - una real
-- raíces 2 0 1 - dos complejas
-- raíces 0 1 1 - no es de segundo grado

raíces' :: Double -> Double -> Double -> (Double, Double)
raíces' a b c
  | a == 0 = error "raíces': la ecuación no es de segundo grado"
  | disc < 0 = error "raíces': las raíces son complejas"
  | otherwise = ((-b + raizDisc) / dosA,
                 (-b - raizDisc) / dosA)
  where
    disc = b*b - 4*a*c
    raizDisc = sqrt disc
    dosA = 2*a

-- regla de sangrado

circArea :: Double -> Double
circArea r = pi*r^2

rectArea :: Double -> Double -> Double
rectArea b h = b*h

circLength :: Double -> Double
circLength r = 2*pi*r

cylinderArea :: Double -> Double -> Double
cylinderArea r h = 2*circ + rect
   where
        circ = circArea r
        l = circLength r
        rect = rectArea l h

-- Definición de operadores

infix ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs(x-y) < 1e-6
