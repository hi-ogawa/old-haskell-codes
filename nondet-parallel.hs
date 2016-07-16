import Data.Maybe (catMaybes)

class Monad m => MonadPlus m  where
  mzero :: m a
  mplus :: m a -> m a -> m a
                          
-- ordinary one
instance MonadPlus [] where
  mzero = []
  mplus = (++)
  
----------------------------------------------------
-- represent nondeteministic-parallel-computation --
----------------------------------------------------  
newtype L a = L {l :: [a]} deriving Show

instance Functor L where
  fmap f = L . map f . l

instance Monad L where
  return = L . (:[])
  (L xs) >>= f = L . cart $ map (l . f) xs
    where cart :: [[b]] -> [b] -- joining list like enumerate cartesian product of coutable sets
          cart yss = catMaybes $ map (choose yss) nat_pair'
          choose :: [[b]] -> (Int, Int) -> Maybe b
          choose yss (x, y) | index_check yss x && index_check (yss !! x) y = Just $ (yss !! x) !! y
                            | otherwise                                     = Nothing
          index_check :: [a] -> Int -> Bool
          index_check []     _             = False
          index_check (x:xs) i | i <= -1   = False
                               | i == 0    = True
                               | otherwise = index_check xs (i-1)

nat_pair' :: [(Int, Int)] -- this is an primitive pairs for monad definition
nat_pair' = (0,0):map f nat_pair'
  where f (0, x) = (x+1, 0)
        f (x, y) = (x-1, y+1)
                                             




-- monadplus definition is ordinary one
instance MonadPlus L where  
  mzero = L []
  xs `mplus` ys = L $ l xs ++ l ys

-- nats
nats :: MonadPlus m => m Int
nats = return 0 `mplus` do i <- nats
                           return (i+1)

-- nat_pair
nat_pair :: MonadPlus m => m (Int, Int)
nat_pair = do x <- nats
              y <- nats
              return (x, y)

-- nat_lists
nat_lists :: MonadPlus m => m [Int]
nat_lists = return [] `mplus`  do x <- nats
                                  xs <- nat_lists
                                  return $ x:xs

------------------------------------------------
-- implementing algolithmic (not using Monad) --
------------------------------------------------

nat_lists' :: [[Int]]
nat_lists' = [] : map f nat_pair'
  where f :: (Int, Int) -> [Int]
        f (n, m) = g (n+1) !! m
        
g :: Int -> [[Int]]
g 0 = undefined
g 1 = map (:[]) [0..]
g n = cartesian (g 1) gn'
  where gn' = g $ n-1
        
cartesian :: [[a]] -> [[a]] -> [[a]]
cartesian xs ys = 
  map (\(x, y) -> (xs !! x) ++ (ys !! y)) nat_pair'


{-
[]
[0],   [1],   [2], ..
[0,0], [1,0], [0,1], [2,0], [1,1], [0,2], [3,0], ..
[0,0,0], [1,0,0]
-}


