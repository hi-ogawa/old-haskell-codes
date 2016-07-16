-- example of < http://jutememo.blogspot.jp/2011/05/haskell-cps.html >

import Control.Monad.Cont
import Data.Char (digitToInt, intToDigit)

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

leafCount :: Tree a -> (Int -> b) -> b
leafCount (Leaf e) k = k 1
leafCount (Branch l r) k = leafCount l $ (\x -> leafCount r $ (\y -> k $ x+y))

t = Branch 
    (Branch 
     (Leaf 1) 
     (Leaf 2))
    (Branch
     (Leaf 3)
     (Branch 
      (Leaf 4)
      (Leaf 5)))
    
fibk n k | n == 0 || n == 1 = k 1    
         | otherwise = fibk (n-1) $ (\x -> fibk (n-2) $ (\y -> k $ x+y))
                       
                       
flatten :: [[a]] -> [a]
flatten [] = []
flatten (xs:xss) = xs ++ flatten xss

flattenk [] k = k []
flattenk (xs:xss) k = flattenk xss $ \l -> k (xs ++ l)

flattenk' [] k = k []
flattenk' ([]:_) k = []
flattenk' (xs:xss) k = flattenk' xss $ \l -> k (xs ++ l)

flattenk'' lss = sub lss []
  where sub [] acc       = acc
        sub ([]:_) acc   = []
        sub (xs:xss) acc = sub xss $ acc ++ xs
        
flattenk''' lss = fst $ foldl sub ([],True) lss
  where sub (ac, bo) xs | not bo || xs == [] = ([], False)
                        | otherwise          = (ac++xs, True)
                                               

myfoldr f z [] = z                                                
myfoldr f z (x:xs) = f x $ myfoldr f z xs

foldrk f z []     k = k z
foldrk f z (x:xs) k = foldrk f z xs (\re -> k $ f x re)

foldrk' _ z []     _ _  k = k z
foldrk' f z (x:xs) p er k | p x            = foldrk' f z xs p er (\re -> k $ f x re)
                          | otherwise      = er
                                          
flat lss = foldrk' (++) [] lss (/=[]) [] id                                          

foldrk2 fk z []     k = k z
foldrk2 fk z (x:xs) k = foldrk2 fk z xs (\re -> fk x re k)

foldrk3 fk z []     k = k z
foldrk3 fk z (x:xs) k = fk x (foldrk3 fk z xs k) (\re -> k re)


plusk x y k = k $ x+y

test2 = foldrk2 plusk 1 [2..10] id
test3 = foldrk3 plusk 1 [2..10] id

test2' = foldrk2 plusk 1 [2..10] (*2)
test3' = foldrk3 plusk 0 [1,2,3] (*2)

--definition : Continuation Monad in Haskell

-- newtype Cont r a = Cont {runCont :: (a -> r) -> r}

-- instance Monad (Cont r) where
--   return a      = Cont $ \k -> k a
-- -- (>>=) :: Cont((a -> r) -> r) -> (a -> Cont((b -> r) -> r)) -> Cont((b -> r) -> r)
-- -- (>>=) :: Cont r a            -> (a -> Cont r b)            -> Cont r b  
--   Cont c >>= f = Cont $ \k -> c (\a -> runCont (f a) k)

--  (>>=) のイメージ
-- Intを貰ってくれる関数を探す <bind>  Intを引き継いで、???を貰ってくれる関数待ち
-- ^^^^^^^^^^^^^^^^             
-- (Int->r)->r               <bind>  (Int-> ((???->r)->r))  ==>  (???->r)->r
-- ^^^^^^                             ^^^^^  ^^^^k^^^            ^^^^k^^^


add_c :: Int -> Int -> Cont r Int
add_c x y = return (x+y)

square_c :: Int -> Cont r Int
square_c x = return (x^2)

bikkuri_c :: Int -> Cont r Char
bikkuri_c x = return (['a'..]!!x)

test_c :: Cont r Int
test_c = add_c 4 5 >>= square_c

test_c2 = do ad1 <- add_c 4 5
             ad2 <- add_c 5 6
             return (ad1+ad2)
-- do notation             
-- add_c 4 5 >>= (\ad1 -> add_c 5 6 >>= (\ad2 -> return (ad1+ad2)))

  
-- -- callCC :: ((a -> Cont((b -> r) -> r)) -> Cont((a -> r) -> r)) -> Cont((a -> r) -> r)
-- callCC f = Cont $ \k' -> runCont (f (\a -> Cont (\_ -> k' a))) k'

bar :: Cont r Int
bar = callCC $ \k -> do
  let n = 5
  k n
  return 25
  
-- f :  \k' -> (k' 5 >>= (\_ -> return 25))
  
------------------------
  
-- callCC f = Cont $ \k -> runCont (f (\a -> Cont (\_ -> k a))) k

-- これに runCont して idを与える。簡約すると..

-- runCont (f (\a -> Cont (\_ -> id a))) id

-- fを適用する (k', aを簡約?)           --fを代入すれば理解しやすい!!

-- runCont (Cont (\_ -> 5) >>= (\_ -> return 25)) id
-- 継続が無視される (5 が返る)

foo :: Int -> Cont r String
foo n =
  callCC $ \k -> do
    let n' = n ^ 2 + 3
    when (n' > 20) $ k "over twenty"
    return (show $ n' - 4)
    
foo' n =    
  callCC $ \k -> do let n' = n ^ 2 + 3
                    if (n' > 20) 
                      then do k "over twenty"
                      else do return (show $ n' - 4)
-- foo' n =                             
-- Cont $ \k' -> runCont (do let n' = n ^ 2 + 3
--                           if (n' > 20) 
--                             then do Cont (\_ -> k' "over twenty")
--                             else do return (show $ n' - 4) ) k'
                 


{- Continuation モナドを使って，コードブロックからの「脱出」を行います
   この関数は以下のような処理の複雑な制御構造を実装します

   Input (n)       Output                       List Shown
   =========       ======                       ==========
   0-9             n                            none
   10-199          number of digits in (n/2)    digits of (n/2)
   200-19999       n                            digits of (n/2)
   20000-1999999   (n/2) backwards              none
   >= 2000000      sum of digits of (n/2)       digits of (n/2)
-}  
fun :: Int -> String
fun n = (`runCont` id) $ do
        str <- callCC $ \exit1 -> do                        -- "exit1" を定義
          when (n < 10) (exit1 (show n))
          let ns = map digitToInt (show (n `div` 2))
          n' <- callCC $ \exit2 -> do                       -- "exit2" を定義
            when ((length ns) < 3) (exit2 (length ns))
            when ((length ns) < 5) (exit2 n)
            when ((length ns) < 7) $ do let ns' = map intToDigit (reverse ns)
                                        exit1 (dropWhile (=='0') ns')  --escape 2 levels
            return $ sum ns
          return $ "(ns = " ++ (show ns) ++ ") " ++ (show n')
        return $ "Answer: " ++ str