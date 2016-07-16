import Control.Arrow

-- cycle, arrow version
ex0 = loop (snd &&& uncurry (++))

-- repeat, arrow version
ex1 = loop (snd &&& uncurry (:))

-- fib, ordinary version
fibs0 = 1 : 1 : (zipWith (+) fibs0 (tail fibs0))

-- ex2 = loop $ snd >>> id&&&(id&&&tail >>> uncurry (zipWith (+)) >>> (1:) >>> (1:))

{-
loop (snd &&& g)

loop (snd && g) x
==>
let x' = y
    y  = snd $ g (x, y)
    y  = 1:1:(zipWith (+) y (tail y))
-}
fibs1 :: a -> [Int]
fibs1 = (loop (snd &&& g))
  where g = snd >>> (id &&& tail) >>> (uncurry $ zipWith (+)) >>> ([1,1]++)

-- ordinary fizz-buzz
fizzbuzz :: [String]
fizzbuzz = map f [0..]
  where f i | i `mod` 3 == 0 && i `mod` 5 == 0 = "fizz buzz"
            | i `mod` 3 == 0                   = "fizz"
            | i `mod` 5 == 0                   = "buzz"
            | otherwise                        = show i 

-- arrow fizz-buzz (conditional branch exercise)
fizzbuzz1 :: [String]
fizzbuzz1 = map f [0..]
  where f = br >>> (const "fizz buzz"||| 
                    const "fizz"||| 
                    const "buzz"|||
                    show)
        br i | i `mod` 3 == 0 && i `mod` 5 == 0 = Left ()
             | i `mod` 3 == 0                   = Right (Left ())
             | i `mod` 5 == 0                   = Right (Right (Left ()))
             | otherwise                        = Right (Right (Right i))
                                                  
fact :: Int -> Int
fact = br >>> (id ||| (id &&& (subtract 1 >>> fact) >>> uncurry (*)))
  where br i | i <= 1    = Left 1
             | otherwise = Right i
