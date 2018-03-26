--Ejercicio 3
power4_a::Int->Int
power4_a x= x*x*x*x
power4_b::Int->Int
power4_b x= sqr (sqr x) where sqr y = y*y 

--Ejercicio 4
fib 1 = 1 
fib 0 =1 
fib n = fib(n-1)+fib(n-2)

--ejercicio 9
es_bisiesto x= (mod(mod x 100)4==0 && mod x 100 /= 0) || mod x 400 == 0

--ejercicio 10
between x y z= if x< (max x (max y z )) && x > (min x (min y z )) then x else if y< (max x (max y z )) && y > (min x (min y z )) then y else z
sort3 x y z= (min x (min y z),between x y z,max x (max y z))