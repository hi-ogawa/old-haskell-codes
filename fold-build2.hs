-------------------------------------------
-- 2014/7/25 : fold/build categorically ---
-------------------------------------------

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

{-
foldr :: (a -> b -> b) -> b -> ([a] -> b)
(foldr f k :: [a] -> b)
foldr f k []    = k
foldr f k (a:l) = a `f` (foldr f k l)

foldr f k (build g)  ==  g f k
-}

-- ordinary definition
repeatn :: Int -> a -> [a]
repeatn 0 x = []
repeatn n x = x : (repeatn (n-1) x)

-- using build function ver.
repeatn' :: Int -> a -> [a]
repeatn' n x = build (\c z -> repeatnFB n x c z)

repeatnFB :: Int -> a -> (a -> [a] -> [a]) -> ([a]) -> [a]
repeatnFB 0 x c z = z
repeatnFB n x c z = x `c` (repeatnFB (n-1) x c z)


{- ex. (reduction of "sum repeatn 2 10")
( "==" inline expansion)
( "==>" reduction (call by name))

<1>
foldr (+) 0 (repeatn' 2 10)
==  foldr (+) 0 (build (\c z -> repeatnFB 2 10 c z))
==  (\c z -> repeatnFB 2 10 c z) (+) 0
==> repeatnFB 2 10 (+) 0
==> 10 `+` (repeatnFB 1 10 (+) 0)
==> 10 `+` (10 `+` (repeatnFB 0 10 (+) 0))
==> 10 `+` (10 `+` 0)
==> 20

<2>
foldr (+) 0 (repeatn 2 10)
==> foldr (+) 0 (10 : (repeatn 1 10))       (~ pattern matching the 3rd argument of "foldr")
==> 10 `+` (foldr (+) 0 (repeatn 1 10))     (~ reduce "foldr" by 2nd case)
==> 10 `+` (foldr (+) 0 (10 : (repeatn 0 10)))    (~ similarly)
==> 10 `+` (10 `+` (foldr (+) 0 (repeatn 0 10)))  (~ similarly)
==> 10 `+` (10 `+` (foldr (+) 0 []))              (~ reduce "repeatn" by 1st case)
==> 10 `+` (10 `+` 0)                             (~ reduce "foldr" by 1st case)
==> 20

the numbers of times of programs-recursive-call are same.
however, <2> needs intermediate list structure for (10 : (10 : []))
while <1> does not.
-}
