--ejercicio1
  --a
  --True
  --b
  --(5,7)
  --c
  ej2_1c x= if x=='h' then 0 else 1
  --d
  ej2_1d (x,y)='a'>y || x<1 
  --e
  ej2_1e sum =  1+(sum 1) 
  --f
  --((True&&),6)
  --g
  ej2_1g x = True
  --h
  ej2_1h x=x

--ejercicio 4
  --a first::(a,b)->a
  first (x,y) = x
  --b second::(a,b)->b
  second (x,y) = y
  --c const::a->b->a
  consT x y = x
  --d compose::(b->c)->(a->b)->(a->c)
  compose f g = (\x -> f (g x))
  --e apply::(a->b)->a->b
  apply f x = f x
  --f subst::(a->b->c)->(a->b)->a->c
  subst f g x = f x (g x)
  --g pairFunc::(a->b,b->a)->b->a->(b,a)
  pairFunc (f1,f2) x y = (f1 (f2 x), f2 (f1 y))
  
--ejercicio 6 
  --a Correcto
  --b Incorrecto, todo if debe tener else
  --c  Incorrecto, := es sintacticamente incorrecto
  --d Incorrecto, error de tipos
  --e Correcta
  --f incorrecta, las funciones de mayor, menor, etc son binarias

--ejercicio 9
  --a undefined
  --b 
  ej2_9b x=if (x+1)<0 then undefined else undefined
  --c
  ej2_9c x=undefined
  --d

--ejercicio 10
  --La funcion tom x=x x no es correcta porque al definir el tivo vemos que entra en un loop recursivo sin fin

--ejercicio 11
  --a
  smaller::Int->Int->Int->Int
  smaller = \x->(\y->(\z->if (x<y) && (x<z) then x else if (y<x) && (y<z) then y else z))
  --b
  second_2:: a->b->b
  second_2 = \x->(\x->x)
  --c
  andThen::Bool->Bool->Bool
  andThen= \x->(\z->if x then z else x)

--ejercicio 12
  --a
  iff x y | x = not y
          | not x = y
  --b
  alpha x y = y

--ejercicio13
  bhaskara:: Float->Float->Float->(Maybe Float,Maybe Float)
  bhaskara a b c | (b*b-4*a*c)>=0 = (Just ((-b+sqrt (b*b-4*a*c))/2*a),Just ((-b+sqrt (b*b-4*a*c))/2*a)) 
                 | (b*b-4*a*c)<0 = (Nothing,Nothing)