--ejercicio 1 
  --validas: a,b,c,d,i
  --invalidas: e,h,k
--ejercicio2		
	sum []=0
	sum x:xs= x+sum xs

	any []=False
	any x:xs= x || (any xs)

	all []= False
	all x:[]= x || (all [])
	all x:xs= x && (all xs)

	codes [] = []
	codes x:xs = (Data.Char.ord x):(codes xs)

	remaindes [] y =[]
	remainders x:xs y = (mod x y):(remainders xs y)

	squares [] =[]
	squares x:xs = (x*x):(squares xs)

	lengths []= []
	lengths x:xs = (length x):(length xs)
	length []=0
	length x:xs= 1+length xs

	order []=[]
	order (a,b):xs=if a<(3*b) then (a,b):(order xs) else order xs

	pairs []=[]
	pairs x:xs= if mod x 2 == 0 then x:(pairs xs) else pairs xs

	chars []=[]
	chars x:xs= if x>='A' && x<='z' then x:(chars xs) else chars xs

	moreThan [] n=[]
	moreThan x:xs n= if length x > n then x:(moreThan xs n) else moreThan xs n

--ejercicio 3
	--si, ya que en las definiciones no recursivas devuelve un resultado concreto 
	--y en las recursivas se le quita un elemento a la estructura hasta que en algún momento 
	--se hace pattern matching con la definicion no recursiva que es el caso base

--ejercicio 4 

  --a falsa (xs debe ser una lista de listas) el resultado sería [[],*el contenido de xs*]
  --b verdadero si xs es una lista
  --c falso, igual que a
  --d xs debería ser una lista vacía, y sería falsa, el resultado sería []:[]
  --e falso, se explicó en b
  --f verdadero 
  --g Verdadero en cualquier caso
  --i false
  --j verdadero

--ejercicio 9
--b
  listaImpares []=[]
  listaImpares x:xs= listaImpares xs ++ impares x

  impares []=[]
  impares x:xs= if ((mod x 2) == 1) then x:(impares xs) else impares xs

--ejercicio 10
--a
  data DigitBin = Cero | Uno
  suma_digBin x Cero= x
  suma_digBin Cero y= y
  suma_digBin x y= Cero

  prod_digBin Uno Uno = Uno
  prod_digBin x y = Cero

  mod2_digBin x=x
  carry:: DigitBin->DigitBin->DigitBin
  carry Uno Uno= Uno
  carry x y= Cero
  data NumBin = [DigitBin]
  sumaBin x:xs []= x:xs
  sumaBin [] x:xs= y:ys
  sumaBin x:xs y:ys= (suma_digBin x y):sumaBin (sumaBin xs [carry x y]) ys

  doubleBin x= sumaBin x x
  module2Bin x:xs= mod2_digBin x
  div2 x:xs= xs