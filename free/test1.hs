{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
-- http://d.hatena.ne.jp/fumiexcel/20121111/1352614885

import Control.Monad.Fix (MonadFix, mfix, fix)

data Fix f = Fix (f (Fix f))

data Free f a = Pure a 
              | Free (f (Free f a))
                
instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free c) = Free (fmap (fmap f) c)

instance Functor f => Monad (Free f) where
  return x = Pure x
  (Pure x) >>= k = k x
  (Free c) >>= k = Free (fmap (>>=k) c)
                
data CharIO next = GetCh (Char -> next)
                 | PutCh Char next
                   
instance Functor CharIO where
  fmap f (GetCh cont)     = GetCh (f . cont)
  fmap f (PutCh ch cont)  = PutCh ch (f cont)

instance MonadFix (Free CharIO) where
  mfix (f :: a -> Free CharIO a) = fix rec
    where rec :: Free CharIO a -> Free CharIO a
          rec (Pure x)           = f x
          rec (Free (GetCh g))   = Free . GetCh $ \c -> rec (g c)
          rec (Free (PutCh c n)) = Free $ PutCh c (rec n)

data State next = State (Int -> (next, Int))

instance Functor State where
  fmap f (State g) = State $ \i -> let (a, i') = g i in (f a, i')
  
instance MonadFix (Free State) where
  mfix (f :: a -> Free State a) = 
    fix $ \fx -> case fx of
      Pure x -> f x
      Free (State g) -> Free . State $ \i -> let (fx', i') = g i in ()
    
    where rec :: Free State a -> Free State a
          rec (Pure x) = f x
          rec (Free (State g)) = Free . State $ \i -> let (fx, i') = g i in (rec fx, i')
{-
g :: Int -> (Free State a)

f :: a -> StateT s m a
          ^^^^^^^^^^^^ s -> m (a, s)
instance (MonadFix m) => MonadFix (StateT s m) where
    mfix f = StateT $ \s -> mfix $ \ ~(a, _) -> runStateT (f a) s
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  (a, s) -> m (a, s)
           = StateT $ \s -> fix $ \~(a, _) -> (f a) s
-}

type GenericIO a = Free CharIO a
  
ex0 :: GenericIO ()  
ex0 = do mapM_ putCh "\ntest-start\n"
         ch <- getCh
         putCh ch
         mapM_ putCh "\ntest-end\n"
         
putCh :: Char -> Free CharIO ()
putCh ch = Free $ PutCh ch (Pure ())

getCh :: Free CharIO Char
getCh = Free $ GetCh (\ch -> Pure ch)

runCIO :: Free CharIO a -> IO a
runCIO (Pure x) = return x
runCIO (Free (GetCh cont1)) = do ch <- getChar 
                                 runCIO $ cont1 ch
runCIO (Free (PutCh ch cont2)) = putChar ch >> runCIO cont2
-- cont1 :: Char -> Free CharIO a
-- cont2 :: Free CharIO a

-- 最も簡単なex1がどう展開されるか考察してみる
e0 = do ch <- getCh
        putCh ch
         
-- まずdo構文をなくす
e1 = getCh >>= (\ch -> putCh ch)
e1' = (Free chio)
      >>= (\ch -> putCh ch)
  where chio = GetCh (\ch -> Pure ch)
    
-- bind展開(`Free Chio >>=`)
e2 = Free $ fmap (>>=(\ch -> putCh ch)) chio
  where chio = GetCh (\ch -> Pure ch)

-- fmap展開(`fmap .. (GetCh a)`)
e3 = Free $ GetCh ((>>=(\ch -> putCh ch)) . (\ch -> Pure ch))
e3'= Free $ GetCh (\ch -> Pure ch >>= \ch -> putCh ch)

-- bind展開(`Pre ch >>=`)
e4 = Free $ GetCh (\ch -> putCh ch)
e4'= Free $ GetCh (\ch -> Free $ PutCh ch (Pure ()))

-- で、これをrunする。
e5 = runCIO e4'
e5'= do ch <- getChar
        runCIO (Free $ PutCh ch (Pure ()))
        
e5''= do ch <- getChar        
         putChar ch
         return ()