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

data GenTree a = Gen a [GenTree a]

foldGen :: (a -> [b] -> b) -> GenTree a -> b
foldGen f (Gen x gs) = f x (map (foldGen f) gs)

foldGen' :: (a -> c -> b) -> ([b] -> c) -> GenTree a -> b
foldGen' f g (Gen x gs) = f x (g (map (foldGen' f g) gs))

foldGen'' :: (a -> c -> b) -> ([b] -> c) -> GenTree a -> b
foldGen'' f g = foldGen (\a bs -> f a (g bs))