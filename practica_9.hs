--ejercicio 2
data BinTree a = Empty
               | Bin a (BinTree a) (BinTree a)
               deriving Show

foldBin :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldBin f z Empty = z
foldBin f z (Bin x t1 t2) = f x (foldBin f z t1) (foldBin f z t2)

mapBin :: (a -> b) -> BinTree a -> BinTree b
mapBin f = foldBin (Bin . f) Empty

heightBin :: BinTree a -> Int
heightBin = foldBin (\x h1 h2 -> 1 + max h1 h2) 0

mirrorBin :: BinTree a -> BinTree a
mirrorBin = foldBin (flip . Bin) Empty
--ejercicio 3
data GenTree a = Gen a [GenTree a]

foldGen :: (a -> [b] -> b) -> GenTree a -> b
foldGen f (Gen x gs) = f x (map (foldGen f) gs)

foldGen' :: (a -> c -> b) -> ([b] -> c) -> GenTree a -> b
foldGen' f g (Gen x gs) = f x (g (map (foldGen' f g) gs))

foldGen'' :: (a -> c -> b) -> ([b] -> c) -> GenTree a -> b
foldGen'' f g = foldGen (\a bs -> f a (g bs))

--ejercicio 4
data GenExp a = Leaf a | Un (GenExp a) | BinG (GenExp a) (GenExp a) deriving Show

foldGenExp :: (a -> b) -> (b -> b) -> (b -> b -> b) -> GenExp a -> b 
foldGenExp l u b (Leaf x)=l x
foldGenExp l u b (Un t1)=u (foldGenExp l u b t1 )
foldGenExp l u b (BinG t1 t2)=b (foldGenExp l u b t1) (foldGenExp l u b t2) 

data NExp = Num Int| Sum NExp NExp | Sub NExp NExp | Neg NExp deriving Show

foldNExp::(Int->b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> NExp -> b
foldNExp z g h f (Num x) = z x
foldNExp z g h f (Sum n1 n2) = g (foldNExp z g h f n1)(foldNExp z g h f n2)
foldNExp z g h f (Sub n1 n2) = h (foldNExp z g h f n1) (foldNExp z g h f n2)
foldNExp z g h f (Neg n1) = f (foldNExp z g h f n1)

data Nat = Zero | Succ Nat deriving Show

foldNat:: b->(b->b)->Nat-> b
foldNat z f Zero = z
foldNat z f (Succ n) = f (foldNat z f n)