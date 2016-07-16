import Prelude hiding (foldr, map, repeat, iterate)
import GHC.Exts (build, augment)

-- build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
-- build g = g (:) []


-- g :: (a -> b -> b)  -> b  -> b             -> [a] 
--      ^^^^^^^^^cons  ^^[]  ^^gの返り値
-- と行った形で実際に適用される
-- gに(cons,[])を適用して帰ってくる(b)が全体の返り値の([a])になる。

-- iterateをbuildを使って書く
iterate f x = build (\c _n -> iterateFB c f x)
  where iterateFB c f x = x `c` iterateFB c f (f x)

repeat x = build (\c _n -> repeatFB c x)
  where repeatFB c x = let xs = x `c` xs
                       in  xs


-- foldr f n []     = n
-- foldr f n (x:xs) = x `f` (foldr f n xs)

foldr f n = go
  where go []     = n
        go (x:xs') = x `f` (go xs')
        
-- mapはbuildとfoldrを使ってかける

mapFB c f x ys = (f x) `c` ys

map f xs = build (\c n -> foldr (mapFB c f) n xs)

{- fold/build変換規則
foldr k n (build g) = g k n

== foldr k n (g (:) [])

-}
-- これを例にして、fold/build融合変換を試す
sum' = foldr (+) 0
ex0 = sum' $ map (+1) [1..5]

{-
== foldr (+) 0 $ map (+1) [1..5]

== foldr (+) 0 $ build (\c n -> foldr (\e acc -> e+1 `c` acc) n [1..5])

(変換できる!!)
== (\c n -> foldr (\e acc -> e+1 `c` acc) n [1..5]) (+) 0

((+), 0を適用)
== foldr (\e acc -> e+1 + acc) 0 [1..5]

これでfoldrが一つになった。
- 本来、($)の間で受け渡されるはずだった中間構造のリストが必要なくなっている。
  (もとのリスト[1..5]をそのままfoldできる)

* correctness of short cut fusion *

foldr k n (build g)
==> foldr k n (g (:) [])
==> g k n

g = seq
k = undefined
n = 0
とすると同値性が失われる。

foldr k n (build g) ==> foldr k n (g (:) []) ==> foldr k n [] ==> []
g k n ==> seq undefined 0 ==> undefined

<同値になる条件>
k undefined undefined  /=  undefined
かつ
n /= undefined
-}

-- augmentの利用
-- augment :: (forall b. (a -> b -> b) -> b -> b) -> [a] -> [a]
-- augment g xs = g (:) xs

-- appendをaugmentに置き換える
-- append []     ys = ys
-- append (x:xs) ys = x : (append xs ys)
append xs ys = augment (\k n -> foldr k n xs) ys

-- 試しにsumしてみる
ex1 = sum $ append [1..5] $ map (+1) [6..10]

{-
- "foldr/augment" -
foldr k n (augment g xs) = g k (foldr k n xs)
これもやりたいことは分かる。

- "augment/build" -
augment h (build g) = build (\c n -> h c (g c n))

- "augment/nil" - 自明
augment g [] = build g


-- ex1の簡約 --
sum $ append [1..5] $ build (\c n -> foldr (\e acc -> (e+1) `c` acc) n [6..10])
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^g
sum $ augment (\c xs -> foldr c xs [1..5]) (build g)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^h
==>> "augment/build"の書き換え

sum $ build (\c n -> h c (g c n))

foldr (+) 0 $ build (\c n -> h c (g c n))

==>> "fold/build"の書き換え

(\c n -> h c (g c n)) (+) 0

h (+) $ g (+) 0

h (+) $ foldr (\e ac -> e+1 + ac) 0 [6..10]

foldr (+) (foldr (\e ac -> e+1 + ac) 0 [6..10]) [1..5]

最終的にfoldr二回になる。(リストの中間構造はなくなった。)
-}

