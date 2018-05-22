--ejercicio 1
  union:: [a]->[a]->[a]
  union x y= x++y
  interseccion:: [a]->[a]->[a]
  in:: a->[a]->Bool
  in x []= False
  in x y:ys= (x==y) || (in x ys)
  interseccion [] ys=[]
  interseccion x:xs ys= if (in x ys) then x:interseccion xs ys else interseccion xs ys

--ejercicio 2
  data TipTree a= Tip a | Join (TipTree a) (TipTree a) 
  heightTip::TipTree->Int    
  heightTip (Tip a)=0
  heightTip (Join x y) 1+max (heightTip x) (heightTip y)

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

