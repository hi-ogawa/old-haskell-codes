{-
<sample>
3 5 3
3 1 2
=> 1

4 7 2
3 3 2 4
=> 2

5 5 1
1 3 1 2 1
=> -1
-}

import Data.List (sort, inits, find)

main = do (n:m:k:as) <- (map read.words) `fmap` getContents
          let ans = solve n m k as
          print ans
          
-- n    : タコ足の個数
-- m    : 用意すべき電源の数
-- k    : 部屋の電源の数
-- as   : それぞれのタコ足のプラグの数
-- 1 ~ 50
-- return 最低限必要なタコ足の個数          
solve :: Int -> Int -> Int -> [Int] -> Int
solve _ m k as | k == 0    = -1
               | otherwise = (case find (\ts -> (k - length ts) + sum ts >= m ) $ inits as' of
                                 Nothing       -> -1
                                 Just as''     -> length as''
                             )
  where as'  = takeWhile (>=2) . reverse . sort $ as -- 足の多いものから貪欲に, 足一つは必要ない
 