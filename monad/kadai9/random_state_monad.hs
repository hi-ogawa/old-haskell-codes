import System.Random

-- random :: (Random a, RandomGen g) => g -> (a, g)
-- mkStdGen :: Int -> StdGen


-- newtype 
-- the new type and the type of the field are in direct correspondence!! 
-- State :: (s -> (a, s)) -> State s a
-- runState :: State s a -> (s -> (a, s))

newtype State a b = State { runState :: (a -> (b, a)) }
instance Monad (State a) where
  return bx = State $ \ax -> (bx, ax)
  State f1 >>= f2 = State $ \ax -> let (bx,ay) = f1 ax in (runState (f2 bx)) ay
    
get :: State a a
get = State $ \ax -> (ax,ax)
put :: a -> State a ()
put ax = State $ \_ -> ((),ax)

takeRanElem :: RandomGen g => [a] -> State g a
takeRanElem ls = do gx <- get
                    State (randomR (0,length ls -1)) >>= return.(!!) ls 
                    
randomtest = do ls <- mapM takeRanElem $ take 20 $ repeat [1..100]
                ls' <- mapM takeRanElem $ take 20 $ repeat ['a'..'z']
                return $ zip ls ls'
                 
--test :: IO [Int]
test = do newStdGen
          rg <- getStdGen
          let (rls, rg') = runState randomtest rg
          return rls
  
-- algebraic data type  
-- data StateOp s a = Run (s -> (a, s))
-- instance Monad (StateOp s) where
--   --  return :: a -> RanGenOp g a
--   --  (>>=) :: RanGenOp g a -> (a -> RanGenOp g b) -> RanGenOp g b
--   return x = Run (\g0 -> (x, g0))
--   Run f1 >>= f2 = Run (\g0 -> let (x1, g1) = f1 g0
--                                   (x2, g2) = case f2 x1 of Run f2' ->  f2' g1
--                               in (x2,g2))
-- get :: StateOp s s
-- get = Run (\s -> (s,s))
-- put :: s -> StateOp s ()
-- put g = Run (\g0 -> ((), g))
-- test :: StateOp StdGen (Int,Int,Int,Int,Int)
-- test = do i1 <- Run random
--           i2 <- Run random
--           e1 <- takeRanElem [0..100]
--           e2 <- takeRanElem [0..100]
--           e3 <- takeRanElem [0..100]          
--           return (i1,i2,e1,e2,e3)
-- test' = case test of
--   Run f -> f g
--   where g = mkStdGen 100
-- takeRanElem :: [a] -> StateOp StdGen a
-- takeRanElem ls = do g <- get
--                     rIndex <- Run (randomR (0,length ls - 1))
--                     return $ ls !! rIndex
        
                                 


-- comb :: (Random a, RandomGen g)
--         => RanGenOp g a
--         -> (a -> RanGenOp g a)
--         -> RanGenOp g a
-- comb f1 f2 = \g0 -> let (x1, g1) = f1 g0
--                         (x2, g2) = f2 x1 g1
--                     in (x2,g2)

-- ret :: (Random a, RandomGen g)
--        => a -> RanGenOp g a
-- ret x = \g0 -> (x, g0)
