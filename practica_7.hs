import Data.Char
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

data Arbol a = Hoja a | Nodo  a (Arbol a) (Arbol a)
foldArbol' f z (Hoja a)= z a
foldArbol' f z (Nodo a x y)= f a (foldArbol' f z x) (foldArbol' f z y)

--ejercicio 1

sum= foldr' (+) 0
any= foldr' (||) False
all= foldr' (&&) True
codes= foldr' ((:) . ord) []
lengths::[[a]]->[Int]
lengths= foldr' ((:) . length) []
remainders y = foldr' (\x n->(mod x y):n) [] 
order= foldr' (\(x,y) n->if x<(3*y) then (x,y):n else n) []
