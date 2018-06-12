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

--ejercicio 2
hs=foldr' (\x n-> if head x == 'h' then 1+n else n) 0
avgLength x =div (foldr' ((+) . length ) 0 x) (length x)
diffAdj (x:[]) =  []
diffAdj (x:xs) = (x,head xs):diffAdj xs
--ejercicio 3
--f::[a]->[a]
-- f [a,b,c] <-> foldr (:) [] [a,b,c] <-> : a foldr (:) [] [b,c] <->  
-- <-> : a (: b foldr (:) [] []) <-> : a (: b (: c foldr (:) [] [] )) <-> 
-- <-< : a (: b (: c [])) <-> a:b:c:[] <-> [a,b,c]
