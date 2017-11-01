-- <pre><div class="text_to_html">-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 1
--
-- Alumno: FERNÁNDEZ MÁRQUEZ, JOSÉ LUIS
-------------------------------------------------------------------------------

import Test.QuickCheck

-------------------------------------------------------------------------------
-- X Ejercicio 2 - intercambia (HECHO)
-------------------------------------------------------------------------------

intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x) -- completar

-------------------------------------------------------------------------------
-- X Ejercicio 3 - ordena2 y ordena3 (HECHO)
-------------------------------------------------------------------------------

-- 3.a
ordena2 :: Ord a => (a,a) -> (a,a)
ordena2 (x,y) =
  if x <= y then (x,y)
    else (y,x) -- completar

p1_ordena2 x y = enOrden (ordena2 (x,y))
   where enOrden (x,y) = x <= y

p2_ordena2 x y = mismosElementos (x,y) (ordena2 (x,y))
   where
      mismosElementos (x,y) (x',y') =
           (x == x' && y == y') || (x == y' && y == x')

-- 3.b
ordena3 :: Ord a => (a,a,a) -> (a,a,a)
ordena3 (x,y,z) -- completar
 | x==y && x==z = (x,y,z)
 | x<=y && y<=z = (x,y,z)
 | x<=z && z<=y = (x,z,y)
 | y<=x && x<=z = (y,x,z)
 | y<=z && z<=x = (y,z,x)
 | z<=x && x<=y = (z,x,y)
 | otherwise = (z,y,x)

 -- | x>y = ordena3 (y,x,z)
 -- | y>z = ordena3 (x,z,y)
 -- | otherwise (x,y,z)

-- 3.c
p1_ordena3 x y z = enOrden (ordena3 (x,y,z)) -- completar
    where
      enOrden (x,y,z) = x<=y && y<=z

p2_ordena3 x y z = mismosElementos (x,y,z) (ordena3 (x,y,z)) -- completar
    where
      mismosElementos (x,y,z) (x',y',z') =
        (x==x' && y==y' && z==z') || (x==y' && y==x' && z==z') || (x==z' && z==x' && y==y') || (y==z' && z==y' && x==x')

-------------------------------------------------------------------------------
-- X Ejercicio 4 - max2
-------------------------------------------------------------------------------

-- 4.a
max2 :: Ord a => a -> a -> a
max2 x y =  -- completar
    if x>=y then x
      else y

-- 4.b
-- p1_max2: el máximo de dos números x e y coincide o bien con x o bien con y.

p1_max2 x y = (max2 x y) == x || (max2 x y) == y -- completar

-- p2_max2: el máximo de x e y es mayor o igual que x y mayor o igual que y.

p2_max2 x y = (max2 x y) >= x || (max2 x y) >= y -- completar

-- p3_max2: si x es mayor o igual que y, entonces el máximo de x e y es x.

p3_max2 x y =  -- completar
    if x>=y then (max2 x y) == x
      else True

-- p4_max2: si y es mayor o igual que x, entonces el máximo de x e y es y.

p4_max2 x y =  -- completar
    if y>=x then (max2 x y) == y
      else True

-------------------------------------------------------------------------------
-- Ejercicio 5 - entre (resuelto, se usa en el ejercicio 7)
-------------------------------------------------------------------------------

entre :: Ord a => a -> (a, a) -> Bool
entre x (inf,sup) = inf <= x && x <= sup

-------------------------------------------------------------------------------
-- X Ejercicio 7 - descomponer (HECHO)
-------------------------------------------------------------------------------

-- Para este ejercicio nos interesa utilizar la función predefinida:
--
--              divMod :: Integral a => a -> a -> (a, a)
--
-- que calcula simultáneamente el cociente y el resto de la división entera:
--
--   *Main> divMod 30 7
--   (4,2)

-- 7.a
type TotalSegundos = Integer
type Horas         = Integer
type Minutos       = Integer
type Segundos      = Integer

descomponer :: TotalSegundos -> (Horas,Minutos,Segundos)
descomponer x = (horas,minutos,segundos) -- completar
    where
      modulox3600 = mod x 3600
      horas = div x 3600
      minutos = div modulox3600 60
      segundos = mod modulox3600 60

-- 7.b
p_descomponer x = x>=0 ==> h*3600 + m*60 + s == x
                           && m `entre` (0,59)
                           && s `entre` (0,59)
 where (h,m,s) = descomponer x

-------------------------------------------------------------------------------
-- X Ejercicio 14 - potencia
-------------------------------------------------------------------------------

-- 14.a
potencia :: Integer -> Integer -> Integer
potencia b n =
  if n==0 then 1
   else if n==1 then b
    else b * (potencia b (n-1)) -- completar

-- 14.b
potencia' :: Integer -> Integer -> Integer
potencia' b n = -- completar
   if (n==0) then 1
    else if (n==1) then b
    else if (mod n 2 == 0) then npar * npar
       else b*nimpar*nimpar

    where
      npar = potencia' b (div n 2)
      nimpar = potencia' b (div (n-1) 2)

-- 14.c
p_pot b n =
   potencia b m == sol && potencia' b m == sol
   where sol = b^m
         m = abs n
-- 14.d
{-

Escribe en este comentario tu razonamiento:

-}
-- </div></pre>
