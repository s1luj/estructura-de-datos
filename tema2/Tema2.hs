<pre><div class="text_to_html">-------------------------------------------------------------------
-- Estructuras de Datos
-- Grados en Ingeniería Informática, del Software y de Computadores
-- Tema 2. Características de la Programación Funcional
-- Pablo López
-------------------------------------------------------------------

module Tema2 where

import           Data.Char
import           Data.List
import           Test.QuickCheck

-- Listas

{-

Determinar el tipo de las siguientes expresiones:

   [ 1 == 1 + 1, -3 < 3 ]

   [ 1 == 1 + 1, undefined, -3 < 3 ]

   [ error [] ]

   "1,2,3"

   [ ('a', 8), ('t', 6) ]

   [ (True) ]

   ( [True] )

   [ [], [2, 3] ]

   [ ["a"] ]

   [ () ]

   ( [] )

-}

-- funciones sobre listas predefinidas en Prelude

{-

   null

   head

   last

   tail

   init

   take

   drop


    head              tail
     |   ------------------------------
    [x1, x2, x3, ............, xn-1, xn]
     ------------------------------  |
                 init               last

    -------------- + -----------------
         take              drop

   elem / notElem

   ++ (concatenación de listas)
-}

-- el tipo String = [Char]

cadena :: String -- == [Char]
cadena = "una cadena es una lista de Char"

-- listas y recursividad: 'null', 'head', 'tail', 'if then else'

-- La recursividad mediante funciones 'null', 'head' y 'tail' se
-- considera mal estilo y debe evitarse. Es mejor emplear patrones,
-- como se explica más adelante.

-- predefinida como 'length'
longitudIfThen :: [a] -> Integer
longitudIfThen xs =
  if null xs then 0
  else 1 + longitudIfThen (tail xs)

-- predefinida como 'sum'
sumaIfThen :: [Integer] -> Integer
sumaIfThen xs =
  if null xs then 0
  else head xs + sumaIfThen (tail xs)

ordenadaIfThen :: Ord a => [a] -> Bool
ordenadaIfThen xs =
  if null xs then True
  else if length xs == 1 then True
  else head xs <= head (tail xs) && ordenadaIfThen (tail xs)

-- constructores de listas: [] y :

--  [x1, x2, x3 ] = x1 : x2 : x3 : []

-- patrones de listas

{-
    patrón             significado

    xs                 cualquier lista

    []                 lista vacía

    [x]                lista con 1 elemento

    [x1, x2, ... xn]   lista con n elementos

    [x,5,z]            lista con tres elementos, el segundo un 5

    (x:xs)             lista con al menos 1 elemento

    (x:y:ys)           lista con al menos 2 elementos

    (x:7:ys)           lista con al menos 2 elementos, el segundo un 1

    ((x,y):zs)         lista de tuplas con al menos 1 elemento

-}

-- las ecuaciones se aplican en el orden en que aparecen
pat :: [a] -> String
pat []       = "lista vacia"
pat [x]      = "lista con un elemento"
pat (x:y:ys) = "al menos 2 elementos"

-- listas y recursividad: patrones y definición por casos

{-

Recursividad sobre listas con patrones:

  1) caso base: la lista vacía []

  2) caso recursivo: lista con al menos un elemento (x:xs)

Esquema típico de una función recursiva sobre listas:

  La idea clave consiste en suponer que se conoce la solución del
  sufijo o cola, es decir, los elementos por visitar.

       f (x:xs) --------> solTotal (solución para x:xs)
         |                   |
         |                   |
         |                   |
       f (xs)   --------> solCola (solución del sufijo xs)
         |                   |
         .                   .
         .                   .
         .                   .
         |                   |
       f []     --------> solBase (solución del caso base)

  'solBase' es la solución del caso base; es decir, la solución para
  la lista vacía. Normalmente es una constante. Por ejemplo, para la
  función 'inversa' tenemos:

       inversa []     --------> []

  'solCola' es la solución para la el sufijo o cola 'xs'. Podemos
  suponer que conocemos esta solución. Basta plantearse cuánto vale
  para un caso concreto. Por ejemplo, para la función 'inversa', si
  'xs' es '[2,3,4]' tenemos:

      inversa [2,3,4]   --------> [4,3,2]
         |                           |
         .                           .
         .                           .
         .                           .
         |                           |
      inversa []        -------->   [ ]

  'solTotal' es la solución para toda la lista (x:xs). Dado que se
  supone que se conoce 'solCola' basta plantearse cómo cambia la
  solución si tenemos en cuenta la cabeza 'x'. Este es el paso
  creativo: hay que pensar qué operación debemos aplicar a la cabeza
  'x' y la solución de la cola 'solCola' para obtener la solución de
  toda la lista. Para el caso de la función 'inversa', suponiendo que
  'x' es '1', que 'xs' es '[2,3,4]' y que 'solCola' es '[4,3,2]',
  vemos que lo que debemos hacer es añadir 'x' al final de 'solCola':

      inversa [1,2,3,4] --------> [4,3,2,1] (añadir 'x' al final de 'solCola')
         |                           |
         |                           |
         |                           |
      inversa [2,3,4]   --------> [4,3,2]
         |                           |
         .                           .
         .                           .
         .                           .
         |                           |
      inversa []        -------->   [ ]
-}

-- predefinida como 'length'
longitud :: [a] -> Integer
longitud []     =  0
longitud (_:xs) = 1 + longitud xs

-- predefinida como 'sum'
suma :: Num a => [a] -> a
suma []     = 0
suma (x:xs) =  x + suma xs

ordenada :: Ord a => [a] -> Bool
ordenada []       = True
ordenada [_]      = True
ordenada (x:y:ys) = x <= y && ordenada (y:ys)

-- predefinida como 'reverse'
inversa :: [a] -> [a]
inversa []     = []
inversa (x:xs) = inversa xs ++ [x]

-- eficiencia: conteo de reducciones (ver transparencias)

-- eficiencia de 'length'

-- eficiencia de (++)

-- eficiencia de 'reverse'

-- eficiencia y acumuladores

{-

Recursividad con acumulador sobre listas:

  1) caso base: la lista vacía []

  2) caso recursivo: lista con al menos un elemento (x:xs)

Esquema típico de una función recursiva con acumulador sobre listas:

  La idea clave consiste en mantener un acumulador 'ac' que contenga
  la solución para el prefijo de los elementos que ya hemos visitado.

          xs               ac

    f (x1:x2:xs)         acBase (solución del caso base)
           |                |
           |                |
    f   (x2:xs)           acX1 (solución hasta x1)
           |                |
           |                |
    f     (xs)            acX2 (solución hasta x2)
           |                |
           .                .
           .                .
           .                .
           |                |
    f     []              acXn (solución hasta Xn, toda la lista)


  'acBase' es la solución del caso base; es decir, la solución para la
  lista vacía. Normalmente es una constante. Este es el valor inicial
  del acumulador, pues aún no se ha visitado ningún elemento. Por
  ejemplo, para la función 'inversa' si 'xs' es '[1,2,3,4]' tenemos:

             xs               ac

       inversa [1,2,3,4]      []

  'acX1' es la solución para el prefijo que contiene el primer
  elemento. Podemos suponer que conocemos esta solución. Basta
  plantearse cuánto vale para un caso concreto. Por ejemplo, para la
  función 'inversa', si 'xs' es '[1, 2,3,4]' tenemos:

             xs               ac

       inversa [1,2,3,4]      []
              |                |
              |                |
       inversa [2,3,4]        [1]


  'acX2 es la solución para el prefijo que contiene los dos primeros
  elementos. Dado que se conoce 'acX1' basta plantearse cómo cambia
  la solución si tenemos en cuenta el siguiente elmento 'x2'. Este es
  el paso creativo: hay que pensar qué operación debemos hacer para
  tener en cuenta 'x2'.  Para el caso de la función 'inversa',
  suponiendo que 'x1' es '1', 'x2' es '2', que 'xs' es '[3,4]' y que
  'acX1' es '[1]', vemos que lo que debemos hacer es añadir 'x2' al
  principio del acumulador 'acX1' para obtener 'acX2':

             xs               ac

       inversa [1,2,3,4]      []
              |                |
              |                |
       inversa [2,3,4]        [1]
              |                |
              |                |
       inversa [3,4]         [2,1]  (añadir 'x2' al principio de 'acX1')

  'acXn' es la solución para toda la lista. En este caso, el
  acumulador contiene la solución total. Para la función 'inversa', si
  'xs' es '[1, 2,3,4]' tenemos:

             xs               ac

       inversa [1,2,3,4]      []
              |                |
              |                |
       inversa [2,3,4]        [1]
              |                |
              |                |
       inversa [3,4]         [2,1]
              .                .
              .                .
              .                .
       inversa []           [4,3,2,1]
-}

-- predefinida como 'length'
longitud' :: [a] -> Int
longitud' xs = longitudAc xs 0
  where
    longitudAc [] ac     = ac
    longitudAc (x:xs) ac = longitudAc xs (1 + ac)

-- predefinida como 'reverse'
inversa' :: [a] -> [a]
inversa' xs = inversaAc xs []
  where
    inversaAc  []      ac =  ac
    inversaAc  (x:xs)  ac = inversaAc xs (x : ac)

-- secuencias aritméticas

-- [inf .. sup]
-- [inf, sig .. sup]
-- [inf ..]
-- [inf, sig ..]

-- definiciones por comprensión

-- { (x,y) | (x,y) pertenece a (R,R) y x^2 + y^2 <= r^2 }

-- generadores: [ expresión | patrón <- expLista ]

generador1 :: [Integer]
generador1 = [ x^2 + 1 | x <- [1..10] ]

generador2 :: [[Integer]]
generador2 = [ zs | (x:y:zs) <- [ [], [1], [1,2], [1,2,3], [1..4], [1..5]] ]

generador3 :: String
generador3 = [ y | (2,y) <- [(2,'a'), (1, 'r'), (1+1, 't'), (0,'y')] ]

potenciasDe2 :: [Integer]
potenciasDe2 = [ 2^x  | x  <- [0..] ]

-- guardas: [ expresión | patrón <- expLista, expBool ]

guarda1 :: [Integer]
guarda1 = [ x^2 + 1 | x <- [1..100], even x ]

guarda2 :: [Integer]
guarda2 = [ x^2 + 1 | x <- [1..100], x `mod` 2 == 0 , x `mod` 3 == 0 ]

-- > vocalesDe "hola mundo"
-- "oauo"
vocalesDe :: String -> String
vocalesDe xs = [ x   |  x <- xs, toLower x `elem` "aeiou" ]

múltiplosDe7QueAcabanEn1024C :: [Integer]
múltiplosDe7QueAcabanEn1024C =
  [ x   |  x  <- [0,7..] ,  x `mod` 10000 == 1024  ]

-- definiciones locales let

ejemploSinLet :: [(Integer, Integer)]
ejemploSinLet = [ (x, x^2 + 1) | x <- [0..10] , even (x^2 + 1) ]

-- reescribir el ejemplo anterior sin calcular x^2 + 1 veces
ejemploConLet :: [(Integer, Integer)]
ejemploConLet = [ (x, y) | x <- [0..10] , let y = x^2 + 1, even y ]

-- generadores anidados

anidados :: [(Integer, Char)]
anidados = [ (x,y) | x <- [1,2,3], y <- "abc" ]

-- tabla de verdad de p => q
implicación :: [(Bool, Bool, Bool)]
implicación = [ (p, q, not p || q) | p <- [False .. True], q<- [False .. True] ]

baraja :: [(String, Int)]
baraja = [ (palo,valor) | palo <- ["oros", "copas", "bastos", "espadas"], valor <- [1..7] ++ [10..12] ]

-- orden superior

-- un valor 'dos' de tipo entero
dos :: Integer
dos = 2

-- puede almacenarse en una tupla, lista, etc.
tuplaConInt :: (Integer, Bool)
tuplaConInt = (dos, True)

listaConInt :: [Integer]
listaConInt = [ 1, dos, 3, dos * dos ]

-- o pasarse como parámetro a una función
invocacionConInt :: Integer
invocacionConInt = signum dos

-- una función 'inc'
inc :: Integer -> Integer
inc x = x + 1

-- un valor 'fun' de tipo función
fun :: Integer -> Integer
fun = inc

-- puede almacenarse en una tupla, lista, etc.
tuplaConFuncion :: (Integer -> Integer, Bool)
tuplaConFuncion = (fun, True)

listaConFuncion :: [Integer -> Integer]
listaConFuncion = [ fun, abs, inc, id ]

-- o pasarse como parámetro a una función

dosVeces :: (Integer -> Integer) -> Integer -> Integer
dosVeces f x = f (f x)

-- Función de orden superior: una función que toma otra función
-- como parámetro o que devuelve una función como resultado.

suma2 :: Integer -> Integer
suma2 x = dosVeces inc x

-- > mapTuple inc inc (1, 7)
-- (2,8)

-- > mapTuple inc chr (1, 65)
-- (2,'A')

-- > mapTuple length not ("hola", True)
-- (4,False)

mapTuple ::  (a -> c ) -> (b -> d ) -> (a, b) -> (c, d )
mapTuple f g (x,y) = (f x, g y)

-- la función map:

{-

'map f xs' aplica una función 'f' a todos los elementos de la lista 'xs'

    map f [  x1,   x2, ...   xn ]
  =          |     |         |
          [ f x1, f x2, ... f xn ]


La lista resultado tiene la misma longitud que la lista de entrada.

-}

cuadrado :: Integer -> Integer
cuadrado x = x * x

-- > cuadrados [0..4]
-- [0,1,4,9,16]
cuadrados :: [Integer] -> [Integer]
cuadrados xs = map cuadrado  xs

-- > aMayúsculas "hola"
-- "HOLA"
aMayúsculas :: String -> String
aMayúsculas xs = map toUpper   xs

-- la lista resultado puede tener un tipo distinto a la lista de entrada

-- > ordinales "java"
-- [106,97,118,97]
ordinales :: String -> [Int]
ordinales xs = map ord   xs

-- > caracteres [106,97,118,97]
-- "java"
caracteres :: [Int] -> String
caracteres xs = map chr  xs

-- todas las funciones definidas con 'map' equivalen a la
-- siguiente recursión
caracteres' :: [Int] -> String
caracteres' []     = []
caracteres' (x:xs) = chr x : caracteres' xs

-- la función filter:
--
--    filter p xs
--
-- selecciona los elementos de la lista xs que verifican el predicado p

vocalesDeF :: String -> String
vocalesDeF xs = filter p xs
  where
    p x = toUpper x `elem` "AEIOU"

-- ejemplo de secuencia infinita con filter
múltiplosDe7QueAcabanEn1024 :: [Integer]
múltiplosDe7QueAcabanEn1024 = filter p [0,7..]
  where
    p x = x `mod` 10000 == 1024

-- lambda-expresiones
--
-- una lambda-expresión es una función anónima
--
-- en la lambda expresión solo aparecen los argumentos
-- y el cuerpo de la función:
--
--             \ args -> expresión

cubo :: Integer -> Integer
cubo x = x * x * x

cubos :: [Integer] -> [Integer]
cubos xs = map cubo xs

-- reemplazamos 'cubo' por una lambda-expresión
cubos' :: [Integer] -> [Integer]
cubos' xs = map (\ x -> x * x * x) xs

vocalesDeL :: String -> String
vocalesDeL xs = filter (\ x -> toUpper x `elem` "AEIOU") xs

-- > múltiplosDe 7 [6, 21, 45, 77, 56, 12]
-- [21,77,56]
múltiplosDe :: Integer -> [Integer] -> [Integer]
múltiplosDe n xs = filter (\ x -> x `mod` n == 0) xs

-- secciones

-- Una sección es un operador binario que recibe un solo
-- argumento, que puede ser el izquierdo o el derecho:
--
--  (+1), (3*), (^2), (2^) ('div' 2)
--
-- La sección espera recibir el otro argumento, por lo que
-- una sección es una función unaria.

-- plegado de listas a la derecha

-- las siguientes funciones calculan un valor que "resume" una lista

-- predefinida como product
-- > producto [1,2,3,4,5]
-- 120
producto :: Num a => [a] -> a
producto []     = 1
producto (x:xs) = (*) x ( producto xs)

-- predefinida como and
-- > conjunción [True, 1 < 2, 'a' == 'a']
-- True
-- > conjunción [True, 1 < 2, 'a' == 'b']
-- False
conjunción :: [Bool] -> Bool
conjunción []     = True
conjunción (x:xs) = (&&) x  (conjunción xs)

-- > esPalabra "haskell"
-- True
-- >esPalabra "haskell2015"
-- False
esPalabra :: String -> Bool
esPalabra []     = True
esPalabra (x:xs) = f x (esPalabra xs)
  where
    f cab solCola = isLetter cab && solCola

-- predefinida como concat
-- aplana [ [1,2], [3], [], [4,5,6]]
-- [1,2,3,4,5,6]
aplana :: [[a]] -> [a]
aplana []       = []
aplana (xs:xss) = (++) xs (aplana xss )

-- las funciones anteriores se aparecen mucho:
-- abstraemos el resultado del caso base, función y tipo

-- predefinida como 'foldr'
recursionLista :: (a -> b -> b) -> b -> [a] -> b
recursionLista f base xs = plegar xs
  where
    plegar []     = base
    plegar (x:xs) = f x (plegar xs)

productoR :: Num a => [a] -> a
productoR xs = foldr (+) 1 xs

conjunciónR :: [Bool] -> Bool
conjunciónR xs = foldr (&&) True xs

esPalabraR :: String -> Bool
esPalabraR xs = foldr f True xs
  where
    f cab solCola = isLetter cab && solCola

aplanaR :: [[a]] -> [a]
aplanaR xs = foldr (++) [] xs

-- Ejercicios de foldr

{-

  esquema general de la recursión sobre listas:

  plegar :: [a] -> b
  plegar [] = base
  plegar (cab:cola) = f   cab     (plegar cola)
                         cabeza  solución de la cola

  ¿Quién es "base""?
  La solución del caso base

  ¿Qué tipo tiene "base"?
  Dado que es una solución, debe ser de tipo 'b'

  ¿Quién es "f"?
  Una función que construye la solución de "cab:cola"
  a partir de la solución de la "cola"

  ¿Qué tipo tiene "f"?
  Dado que "cab" es un elemento de la lista, "cab" es
  de tipo "a". Por su parte, "(plegar cola)" es una
  solución, luego es de tipo "b". Por lo tanto,
  "f" es de tipo "a -> b -> b"

  Ejemplo:

   apariciones 'a' "casa" = foldr f base "casa"


    "casa"   --->   f 'c' 2 = 2

     "asa"   --->   f 'a' 1 = 2

      "sa"   --->   f 's' 1 = 1

       "a"   --->   f 'a' 0 = 1

        []   --->   base = 0

   ¿Qué hace la función "f"?
-}

aparicionesR :: Eq a => a -> [a] -> Integer
aparicionesR x ys = undefined

aparicionesP :: Eq a => a -> [a] -> Integer
aparicionesP x ys = foldr f 0 ys
  where
    f cab solCola
      | cab == x = 1 + solCola
      | otherwise = solCola

{-
  borraTodas 1 [1,2,1,3,1,4] = foldr f base [1,2,1,3,1,4]


   [1,2,1,3,1,4]   --->   f 1 [2,3,4] = [2,3,4]

     [2,1,3,1,4]   --->   f 2 [3,4] = [2,3,4]

       [1,3,1,4]   --->   f 1 [3,4] = [3,4]

         [3,1,4]   --->   f 3 [4] = [3,4]

           [1,4]   --->   f 1 [4] = [4]

             [4]   --->   f 4 [] = [4]

              []   ---->  base = []

-}

borraTodasR :: Eq a => a -> [a] -> [a]
borraTodasR x ys = undefined

borraTodasP :: Eq a => a -> [a] -> [a]
borraTodasP x ys = foldr f [] ys
  where
    f cab solCola
      | cab == x = solCola
      | otherwise = cab : solCola

-- parcialización

f :: Int -> Int -> Int -> Int
f x y z = x + 2*y + 3*z
-- f = \ x -> \ y -> \ z -> x + 2*y + 3*z

-- una función puede recibir menos argumentos de los que espera.
-- se devuelve una función que espera los argumentos que faltan
g :: Int -> Int
g = f 1 2 -- versión especializada de f: g z = 1 + 2*2 + 3*z

-- ejemplo de parcialización

-- sustituye todos los supensos por aprobados
-- > aprobadoGeneral [4, 8.5, 2.5, 7.25, 9.1, 6, 10]
-- [5.0,8.5,5.0,7.25,9.1,6.0,10.0]
aprobadoGeneral :: [Double] -> [Double]
aprobadoGeneral xs = map (max 5) xs

-- composición de funciones

logSeguro :: Double -> Double
logSeguro x = log (abs x)

logSeguro' :: Double -> Double
logSeguro' = log . abs

-- > dentro 1 10 7
-- 7
-- > dentro 1 10 13
-- 10
-- > dentro 1 10 (-2)
-- 1
dentro :: Ord a => a -> a -> a -> a
dentro inf sup x = (max inf . min sup)  x

-- > desplaza 1 'a'
-- 'b'
-- > desplaza 4 'a'
-- 'e'
desplaza :: Int -> Char -> Char
desplaza n x = (chr . (+n) . ord) x

-- tipos algebraicos

-- tipo enumerado: conjunto finito de valores

data Direction = North | South | East | West
                 deriving (Show, Eq, Ord)

{-
-- definición de show (como toString) para Direction

instance Show Direction where
  show North  = "North"
  show South  = "South"
  show East   = "East"
  show West   = "West"

-- definición de Igualdad para Direction

instance Eq Direction where
  North == North  = True
  South == South  = True
  East  == East   = True
  West  == West   = True
  _     == _      = False


-- definición de orden para Direction

instance Ord Direction where
  North  <= _      = True
  South  <= North  = False
  South  <= _      = True
  East   <= North  = False
  East   <= South  = False
  East   <= _      = True
  West   <= North  = False
  West   <= South  = False
  West   <= East   = False
  West   <= _      = True
-}

-- cláusulas deriving

-- tipo unión: cada constructor tiene un argumento

--             Constructor Argumento
data Degrees = Celsius Double
             | Fahrenheit Double
             deriving Show

roomTemp :: Degrees
roomTemp = Celsius 22

roomTemp' :: Degrees
roomTemp' = Fahrenheit 75

frozen:: Degrees -> Bool
frozen (Celsius c)    = c <= 0
frozen (Fahrenheit f) = f <= 32

toCelsius :: Degrees -> Degrees
toCelsius (Celsius c)    = Celsius c
toCelsius (Fahrenheit f) = Celsius ( (f-32) / 1.8)

toFahrenheit :: Degrees -> Degrees
toFahrenheit (Celsius c)    = Fahrenheit (c*1.8 + 32)
toFahrenheit (Fahrenheit f) = Fahrenheit f

-- Tipos algebraicos parametrizados: Maybe y Either

-- data Maybe a = Nothing | Just a

recíproco :: Double -> Maybe Double
recíproco x = if x /= 0 then Just (1 / x)
              else Nothing

nasdaq :: [(String, Double)]
nasdaq = [("intel", 700), ("google", 1000), ("nvidia", 400)]

cotización :: String -> [(String, Double)] -> Maybe Double
cotización empresa mercado = lookup empresa mercado

--          parámetros

-- data Either a b = Left a | Right b

listaMezclada :: [Either Int Bool]
listaMezclada = [Left 1, Right True, Left (5*2), Right (3<1)]

-- tipo producto: un constructor con varios argumentos

--            Cons.     Argumentos
data Persona = P  String String Int deriving Show

-- sinónimos de tipo para mejorar legibilidad
type Name = String
type Surname = String
type Age = Int

-- diferencia entre 'data' y 'type'
-- 'data' introduce un tipo nuevo, diferente de todos los demás
-- 'type' introduce un sinínimo para un tipo que ya existe

-- puedo mezclar 'Name' y 'Surname': son el mismo tipo
compara :: Bool
compara = ("blah" :: Name) == ("blah" :: Surname)

--            Cons.     Argumentos
data Person = Pers  Name Surname Age deriving Show

john :: Person
john = Pers "John" "Smith" 35

name :: Person -> Name
name (Pers nm _ _) = nm

surname :: Person -> Surname
surname (Pers _ snm _) = snm

age :: Person -> Age
age (Pers _ _ ag) = ag

initials :: Person -> (Char, Char)
initials (Pers (n:_) (s:_) _ ) = (n,s)
</div></pre>