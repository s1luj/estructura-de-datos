pat [] = "lista vacia"
pat [x,6,z] = "lista de tres elementos con el segundo un 6"
pat [x,y,z] = "lista de tres elementos"
pat (x:xs) = "lista con al menos un elemento"
pat xs = "cualquier lista"
