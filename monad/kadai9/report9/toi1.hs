--- permute

class Monad m => MonadPlus m  where  
  mzero :: m a
  mplus :: m a -> m a -> m a
                          
instance MonadPlus [] where                    
  mzero = []
  mplus = (++)
  
instance MonadPlus Maybe where  
  mzero = Nothing
  Nothing `mplus` x = x
  x `mplus` _ = x

permute :: MonadPlus m => [a] -> m [a]
permute [] = return []
permute (y:ys) = permute ys >>= (\x -> sub_perm  x [] y)
  where 
    sub_perm li1 li2 x  = if null li1
                          then return (li1 ++ [x] ++ li2)
                          else return (li1 ++ [x] ++ li2) `mplus` (sub_perm li1' li2' x)
                            where
                              li1' = init li1
                              li2' = (last li1):li2
                            

--- nats
nats :: MonadPlus m => m Integer
nats = return 1 `mplus` do i <- nats
                           return (i+1)
                           
--- nat_lists

nat_lists :: MonadPlus m => m [Integer]
nat_lists = return [] `mplus`  do x <- nats
                                  xs <- nat_lists
                                  return $ x:xs
               

{- 実行例
*Main> permute [1,2,3,4] :: [[Int]]
[[4,3,2,1],[4,3,1,2],[4,1,3,2],[1,4,3,2],[4,2,3,1],[4,2,1,3],[4,1,2,3],[1,4,2,3],[2,4,3,1],[2,4,1,3],[2,1,4,3],[1,2,4,3],[3,4,2,1],[3,4,1,2],[3,1,4,2],[1,3,4,2],[3,2,4,1],[3,2,1,4],[3,1,2,4],[1,3,2,4],[2,3,4,1],[2,3,1,4],[2,1,3,4],[1,2,3,4]]

*Main> nats :: [Integer]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,.. (以下無限に続く)..

*Main> nat_lists :: [[Integer]]
[[],[1],[1,1],[1,1,1],[1,1,1,1],[1,1,1,1,1],[1,1,1,1,1,1],[1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1,1,1,1,1.. (以下無限に続く)..

-- 考察
それぞれの関数を実行する際に型を定める必要がある。
Haskellの遅延評価により、nats,nat_listsにおいて、上記の定義が可能である。

すいません、問2以降はできませんでした。

-}