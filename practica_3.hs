--ejercicio1
  --a
    --1
    myand x y = x && y --total
    partialAnd x = and x --parcial
    --2 
    firstOne x y = x
    primary x = firstOne x
    --3
    ej3_1a3 x y z=(x z,y z)
    --4
    ej3_1a4 x y = x+y
    --5
    ej3_1a5 x y = x y + y
    --6
    ej3_1a6 x y z= x z y
--ejercicio2
  --a funo está currificada

--ejercicio5
  --a
    twice = \f->(\x->f(f x))
  --b
    flip = \f->(\x->(\y->f y x))
  --c
    inc2= (+2)
    inc = \x->x+1

--ejercicio6
  --a
  --fix::((a->b)->a->b)->a->b
    fix f x = f (fix f) x
  --b
  --fork (fork,fork) (fork,fork):: 
  --fork::(a->b,a->c)->a->(b,c)
    fork (f,g) x = (f x,g x)
  --c
  --apply apply apply:: ()
  --apply::(a->b)->a->b
    apply f x = f x