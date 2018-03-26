--curry::((a,b)->c)->(a->(b->c))
curry func=f where f a b=func (a,b)

--uncurry::(a->(b->c))->((a,b)->c)
uncurry func=g where g(a,b)=func a b