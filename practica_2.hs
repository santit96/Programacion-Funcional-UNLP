--ejercicio 4
first::(A,B)->A
first (x,y)=x

const:: A->B->A
const x y =x

compose::A->T->T
compose f g= (\x-> f(g x))