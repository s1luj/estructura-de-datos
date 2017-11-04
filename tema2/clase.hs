potenciasDe2 :: [Integer]
potenciasDe2 = [2^x | x <- [1..20]]

generador1 :: [Integer]
generador1 = [x^2+1 | x <- [1..10]]

generador1ConGuarda :: [Integer]
generador1ConGuarda = [x^2+1 | x <- [1..10], even x]

barajaEspanola :: [(String, Int)]
barajaEspanola = [(palo, valor) | palo <- ["oros", "copas", "bastos", "espadas"], valor <- [1..7]++[10..12] ]

cuadrado :: Integer -> Integer
cuadrado x = x*x

dosVeces :: (Integer -> Integer) -> Integer -> Integer
dosVeces f x = f (f x)
