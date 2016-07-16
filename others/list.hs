mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake i (x:xs)  
  | i > 0 = x:(mytake (i-1) xs)
  | otherwise = []
                
myfoldl :: (a->b->a) -> a -> [b] -> a
myfoldl f y [] = y
myfoldl f y (x:xs) = myfoldl f (f y x) xs
  
myfoldr :: (b->a->a) -> a -> [b] -> a
myfoldr f y [] = y
myfoldr f y (x:xs) = f x (myfoldr f y xs)

myfib :: [Int]
myfib = 0:1:(zipWith (+) myfib (tail myfib))

myrepeat :: a -> [a]
myrepeat x = x:(myrepeat x)

mycycle :: [a] -> [a]
mycycle xs = xs ++ (mycycle xs)

myiterate :: (a->a) -> a -> [a]
myiterate f x = x:(myiterate f (f x))

lattice_2_sub n = [(i, n-i) | i <- [0..n]]
lattice_2 n = concat [ lattice_2_sub i | i <- [0..n] ]

lattice_n_sub :: Int -> Int -> [[Int]]
lattice_n_sub 1 n = [[n]]
lattice_n_sub d n = [ i:is | i <- [0..n], is <- lattice_n_sub (d-1) (n-i) ]

lattice_n :: Int -> Int -> [[Int]]
lattice_n d n = concat [ lattice_n_sub d i | i <- [0..] ]

transpose :: [[a]] -> [[a]]
transpose (xs:[]) = [[x] | x <- xs]
transpose (xs:xss) = t_sub xs (transpose xss)
  where t_sub :: [a] -> [[a]] -> [[a]]
        t_sub xs res = zipWith (:) xs res
