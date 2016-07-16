type ID = Int
data Tree a = Node ID (Tree a) (Tree a)
            | Leaf ID a
              
data IState a = IState (ID -> (a, ID))              

instance Functor IState where
  fmap f (IState s) = 
    IState (\i -> let (x,y) = s i
                  in (f x, y))
    
instance Monad IState where  
  return x = IState $ \i -> (x,i)
  join (IState s) = 
    IState (\i -> let (IState t, j) = s i 
                  in t j)

newID = IState (\i -> (i, i+1))


-- import System.Random

-- newtype State s a = State { runState :: (s -> (a,s)) } 
 
-- instance Monad (State s) where 
--     return a        = State $ \s -> (a,s)
--     (State x) >>= f = State $ \s -> let (v,s') = x s in runState (f v) s' 
    
-- class MonadState m s | m -> s where 
--     get :: m s
--     put :: s -> m ()

-- instance MonadState (State s) s where 
--     get   = State $ \s -> (s,s) 
--     put s = State $ \_ -> ((),s) 

-- data MyType = MT Int Bool Char Int deriving Show

-- {- Stateモナドを使うと、乱数を返し、同時に乱数発生器の状態を
--    更新する関数を定義することができます。
-- -}
-- getAny :: (Random a) => State StdGen a
-- getAny = do g      <- get
--             (x,g') <- return $ random g
--             put g'
--             return x

-- -- getAny と同じですが、こちらは返る乱数の範囲を制限します
-- getOne :: (Random a) => (a,a) -> State StdGen a
-- getOne bounds = do g      <- get
--                    (x,g') <- return $ randomR bounds g
--                    put g'
--                    return x

-- {- StdGen を状態として State モナドを使うと、コードを通じて、
--    乱数発生器の状態を一本に繋ぐのを手でおこなうことなく、乱雑な
--    複合型を構築できます
-- -}   
-- makeRandomValueST :: StdGen -> (MyType, StdGen)
-- makeRandomValueST = runState (do n <- getOne (1,100)
--                                  b <- getAny
--                                  c <- getOne ('a','z')
--                                  m <- getOne (-n,n)
--                                  return (MT n b c m))