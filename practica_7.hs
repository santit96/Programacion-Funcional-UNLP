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
pal x= x == (reverse x)
hs=foldr' (\x n-> if head x == 'h' then 1+n else n) 0
avgLength x =div (foldr' ((+) . length ) 0 x) (length x)
adjacents (x:[])= []
adjacents (x:xs)= (x,head xs):adjacents xs
diffAdj (x:[]) =  []
diffAdj (x:xs) = (x,head xs):diffAdj xs
remDups [] = []
remDups (x:xs) = x:remDups (filter (not . (== x )) xs)
primes::Int->[Int]
primes 0 = []
primes n = if (isPrime (abs n)) then n:(primes ((abs n) - 1)) else primes ((abs n)-1)
isPrime:: Int->Bool
isPrime n = dividesAny n (n-1)
dividesAny:: Int->Int->Bool
dividesAny 1 n = False
dividesAny n 1 = True
dividesAny n s = (not (mod n s == 0)) && (dividesAny n (s-1))
--ejercicio 3
--f::[a]->[a]
-- f [a,b,c] <-> foldr (:) [] [a,b,c] <-> : a foldr (:) [] [b,c] <->  
-- <-> : a (: b foldr (:) [] []) <-> : a (: b (: c foldr (:) [] [] )) <-> 
-- <-< : a (: b (: c [])) <-> a:b:c:[] <-> [a,b,c]

--ejercicio 4
filter' c= concat . (map (\x->if (c x) then [x] else []) )
--ejercicio 5 
takewhile::(a->Bool)->[a]->[a]
takewhile c [] = []
takewhile c (x:xs) = if ((length (segment c (x:xs))) > (length (takewhile c xs))) then segment c (x:xs) else takewhile c xs
maxLength::[[a]]->[a]
maxLength = foldr' (\x n -> if (length x) > (length n) then x else n) []
segment::(a->Bool)->[a]->[a]
segment c [] = []
segment c (x:xs)= if (c x) then x:(segment c xs) else []
dropwhile::(a->Bool)->[a]->[a]
dropwhile c [] = []
dropwhile c (x:xs) = if not (c x) then segment (not . c) (x:xs) else dropwhile c xs 

{- ejercicio 6
--a
	Hay que demostrar  map f (xs ++ ys) = map f xs ++ map f ys
	lo haremos por induccion estructural en la estructura de las listas
	- Caso Base xs=ys=[]
	map f ([] ++ [])
	=	por definicion de ++
	map f []
	=	por definicion de map
	[]
	=	por definicion de ++
	[] ++ []
	=	por definicion de map
	map f [] ++ map f []

	- Caso inductivo xs=x:xs' ys=y:ys'
	HI
	map f (xs' ++ ys')== map f xs' ++ map f ys'

	map f (x:xs'++ ys)
	=	por def de append
	map f x:(xs' ++ ys)
	=	por def de map
	f x: map f (xs' ++ ys)
	=	por HI
	f x : (map f xs' ++ map f ys)
	=	por def de append
	f x: (map f xs') ++ (map f ys)
	=	por def de map
	map f x:xs' ++ map f ys

--b
	Demostraremos ahora map f . concat = concat . map (map f)
	concat [] = []
	concat x:xs = x ++ (concat xs)
	por extensionalidad  map f . concat = concat . map (mapf) <->  (map f . concat) xs= (concat . map (mapf)) xs

	Caso base xs = []
	(map f . concat) []
	=	por def de .
	map f (concat [])
	=	 por def de concat
	map f []
	=	 por def de map
	[]
	=	por def de concat
	concat []
	=	por def de map
	concat (map (map f) [])
	=	 por def de .
	(concat . map (map f)) []

	Caso inductivo xs=x:xs'
	
	HI
	(map f . concat) xs' = (concat . map (map f)) xs'

	Tesis inductiva 
	(map f . concat) x:xs' = (concat . map (map f)) x:xs'

	(map f . concat) x:xs'
	=	por def de .
	map f (concat x:xs')
	=	por def de concat
	map f (x ++ concat xs')
	= por ejercicio anterior
	(map f x) ++ map f (concat xs')
	=	por def de .
	map f x ++ (map f . concat) xs'
	= 	por HI
	map x ++ (concat . map (map f )) xs'
	=	por def de .
	map x ++ concat (map (map f ) xs')
	= por def de concat
	concat (map f x : map (map f) xs')
	=	por def de map
	concat (map (map f) x:xs')
	=	por def de .
	(concat . map (map f )) x:xs'

--c
	Vamos a demostrar que filter p (xs ++ ys) = filter p xs ++ filter p ys
	Por propiedad de extensionalidad 
	filter p (xs ++ ys) = filter p xs ++ filter p ys 
	Probaremos por induccion estructural sobre la estructura de las listas

	Caso base xs=[]
	filter p ([] ++ ys) 
	=	por definicion de ++
	filter p ys
	=	por definicion de ++
	[] ++ filter p ys 
	=	por definicion de filter
	filter p [] ++ filter p ys 
	Quedó probado el caso base

	Caso inductivo xs=x:xs'
	HI
	filter p (xs' ++ ys) = filter p xs' ++ filter p ys 

	Tesis inductiva
	filter p (x:xs' ++ ys) = filter p x:xs' ++ filter p ys 

	filter p (x:xs' ++ ys)
	=	por definición de ++
	filter p x:(xs'++ ys)
	=	por definicion de filter 
	if p x then x:filter p (xs' ++ ys) else filter p (xs' ++ ys)
	=	por HI
	if p x then x:(filter p xs' ++ filter p ys)  else filter p xs' ++ filter p ys 
	=	por definicion de ++
	if p x then filter p x:xs' ++ filter p ys else  else filter p xs' ++ filter p ys 

	caso 1 p x = true
	if p x then filter p x:xs' ++ filter p ys else  else filter p xs' ++ filter p ys
	=	por definicion de if then else
	filter p x:xs' ++ filter p ys
	Queda probada la tesis inductivacuando se cumple p x
	
	ahora veamos 
	caso 2 p x = false
	if p x then filter p x:xs' ++ filter p ys else  else filter p xs' ++ filter p ys
	=	por definicion de if then else
	filter p xs' ++ filter p ys

	filter p x:xs' ++ filter p ys 
	= por definicion de filter 
	(if p x then x:filter p xs' else filter p xs') ++ filter p ys
	=	por definicion de if then else y p x=false
	filter p xs' ++ filter p ys
	Queda probada la tesis inductiva cuando p x=false
	por lo tanto probamos que filter p (xs ++ ys) = filter p xs ++ filter p ys

































-}


