  import Data.Char
--ejercicio 1
  union:: [a]->[a]->[a]
  union x y= x++y
  interseccion:: [Int]->[Int]->[Int]
  iN::Int->[Int]->Bool
  iN x []= False
  iN x (y:ys)= ( x == y ) || (iN x ys)
  interseccion [] ys=[]
  interseccion (x:xs) ys= if (iN x ys) then x:interseccion xs ys else interseccion xs ys

--ejercicio 2
  data TipTree a= Tip a | Join (TipTree a) (TipTree a) 
  heightTip::TipTree a->Int    
  heightTip (Tip a)=0
  heightTip (Join x y) = 1 + max (heightTip x) (heightTip y)

  leaves (Tip a)=1
  leaves (Join x y)= leaves x + leaves y

  nodes (Tip a)=0
  nodes (Join x y)=1 + nodes x + nodes y

  walkover(Tip a)=[a]
  walkover (Join x y)= walkover x ++ walkover y

  mirrorTip (Tip a)= Tip a
  mirrorTip (Join x y)= Join (mirrorTip y) (mirrorTip x)

  mapTip func (Tip a)= Tip (func a)
  mapTip func (Join x y) = Join (mapTip func x) (mapTip func y)


-- ejercicio 4 
  data Seq a = Nil | Unit a | Cat (Seq a) (Seq a)

  appSeq Nil x = x
  appSeq x Nil = x
  appSeq x y = Cat x y

  conSeq x Nil = Unit x
  conSeq x y =Cat (Unit x) y

  lenSeq Nil = 0
  lenSeq (Unit x)=1
  lenSeq (Cat x y)=lenSeq x + lenSeq y

  revSeq (Unit x)= Unit x
  revSeq (Cat x y)= Cat (revSeq y) (revSeq x)

  headSeq (Unit x)= x
  headSeq (Cat Nil y)= headSeq y
  headSeq (Cat x y)= headSeq x
    
--ejercicio 5
  data Var = X | Y | Z deriving (Show) 
  data Form = Atom | Or Form Form | Implies Form Form | Forall Var Form | Not Form | And Form Form | Iff Form Form | Exists Var Form deriving (Show) 
  normalize::Form->Form
  normalize Atom = Atom
  normalize (Or x y)= Or (normalize x) (normalize y)
  normalize (Implies x y)= Or (Not (normalize x)) (normalize y)
  normalize (Forall x y)= Not ((Exists x)(Not (normalize y)))
  normalize (Not x)=Not (normalize x)
  normalize (And x y)= Or (Not (normalize x)) (Not (normalize y))
  normalize (Iff x y)=Not (Or (Not (Or (Not (normalize x)) (normalize y))) (Not(Or (Not (normalize y)) (normalize y)) ) )
  normalize (Exists x y)=Exists x (normalize y)