import Control.Monad.Cont
import Control.Applicative (pure, (<*>))
import Data.Char (intToDigit, digitToInt)

-- 普通にCPSっぽく書く
fibC :: Int -> (Int -> a) -> a
fibC 0 c = c 1
fibC 1 c = c 1
fibC n c = fibC (n-1) (\i1 -> fibC (n-2) (\i2 -> c (i1 + i2)))


-- Cont Monadとして作る
fibCM :: Int -> Cont r Int

-- fibCをそのままモナドにくるむ
-- fibCM n = ContT $ \k -> fibC n k

-- monadを利用して定義する(なんかきれい)
fibCM 0 = return 0
fibCM 1 = return 1
fibCM n = do i1 <- fibCM (n-1)
             i2 <- fibCM (n-2)
             return (i1 + i2)
-- fibCM n = pure (\i1 -> \i2 -> i1 + i2) <*> fibCM (n-1) <*> fibCM (n-2)



-- newtype Cont r a = Cont { runCont :: ((a -> r) -> r) } -- r は計算全体の最終の型
--   
-- instance Monad (Cont r) where 
--     return a       = Cont $ \k -> k a                       -- i.e. return a = \k -> k a 
--     (Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k) -- i.e. c >>= f = \k -> c (\a -> f a k) 

-- Contのbindを理解する
factCM 0 = return 1
factCM n = factCM (n-1) >>= (\r -> return (r * n))
--       = \k -> (factCM (n-1)) (\a -> ((\r -> return (r * n)) a) k)
--       = \k -> (factCM (n-1)) (\a -> (return (a * n)) k)
--       = \k -> (factCM (n-1)) (\a -> k (a * n))

-- 型注釈を付けないと
-- factCM :: (Eq a, Monad m, Num a) => a -> m a
-- と推論されるのが面白い
-- モナド則が満たされるならば
-- factCM 10 = return 3628800
-- に簡約できる?

-- *Main> factCM 10 :: [Int]
-- [3628800]
-- *Main> factCM 10 :: Maybe Int
-- Just 3628800
-- *Main> factCM 10 :: Either String Int
-- Right 3628800
-- *Main> runCont (factCM 10) id
-- 3628800

-- runCont (factCM 2) id を簡約してみる (newtypeなコンストラクタは無視)
{-
factCM 2 = factCM 1 >>= (\r -> return (r * 1))
         = (fact 0 >>= (\r -> return (r * 1)) >>= (\r -> return (r * 1))
         = (return 1 >>= \r -> return (r * 1)) >>= (\r -> return (r * 1))

factCM 1 = return 1 >>= (\r -> return (r * 1))
         = (\k -> k 1) >>= (\r -> return (r * 1))
                           ^^^^ f ^^^^^^^^^^^^^^^
         = \k' -> (\k -> k 1) (\a -> (f a) k')
         = \k' -> (f 1) k'
         = \k' -> (return (1 * 1)) k'
         = \k' -> (\k'' -> k'' (1 * 1)) k'
         = \k' -> k' (1 * 1)
-}

-- class (Monad m) => MonadCont m where 
--     callCC :: ((a -> m b) -> m a) -> m a 
--  
-- instance MonadCont (Cont r) where 
--     callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k

-- callCC
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

test :: Int -> Cont r String
test n = do str <- callCC
                   $ \exit1 -> do when (n < 10) (exit1 (show n))
                                  return "error"
            return str

test' :: Int -> Cont r String
test' n = do str <- callCC $ \exit -> exit (show n)
             return str

-- exit1 :: String -> Cont r String
             
{-             
test' 10 = do str <- ..
              return str

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
exit :: (String -> Cont r String)

.. = callCC $ \exit -> exit (show n)
   = Cont $ \k -> runCont ((\exit -> exit (show n)) (\a -> Cont $ \_ -> k a)) k
   = Cont $ \k -> runCont (Cont $ \_ -> k (show n)) k
   = Cont $ \k -> k (show n)
-}
             
{-
--     callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k

callCC (\exit -> do when (n < 10) (exit1 (show n)); return "error")

Cont $ \k -> runCont ((\exit -> .. ) (\a -> Cont $ \_ -> k a)) k
Cont $ \k -> runCont (do when (n < 10) ((\a -> \_ -> k a) (show n)); return "error") k
Cont $ \k -> runCont (do when (n < 10) (Cont (\_ -> k (show n))); return "error") k

Cont $ \k ->
($ k) $ runCont
if (n < 10)
then (Cont (\_ -> k (show n)))
else return ()
>>= \_ -> return "error"

Cont $ \k ->
($ k) $ runCont
if (n < 10)
then (Cont (\_ -> k (show n)))
else (Cont (\k' -> k' ()))
>>= \_ -> return "error"

\k -> ($ k) $ Cont \k' -> (if (n < 10) then (Cont (\_ -> k (show n))) else (Cont (\k'' -> k'' ()))) (\a -> runCont (\_ -> return "error") a k')
\k -> ($ k) $ Cont \k' -> (if (n < 10) then (Cont (\_ -> k (show n))) else (Cont (\k'' -> k'' ()))) (\a -> k' "error")
\k -> (if (n < 10) then (Cont (\_ -> k (show n))) else (Cont (\k'' -> k'' ()))) (\a -> k "error")

例: n = 5の時
\k -> (Cont (\_ -> k (show 5))) (\a -> k "error")
\k -> k (show 5)

(Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k)
-}
