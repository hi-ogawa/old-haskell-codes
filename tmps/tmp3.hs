{-# LANGUAGE GADTs #-}
import Control.Monad
import System.Random
import Data.List (sort)


{- fixing m by []
data Exp a where
  EInt  :: Int  -> Exp Int
  EBool :: Bool -> Exp Bool
  EAdd  :: Exp Int -> Exp Int -> Exp Int
  EEq   :: (Eq a) => Exp a -> Exp a -> Exp Bool
  EIf   :: Exp Bool -> Exp a -> Exp a -> Exp a
  EOr   :: Exp a -> Exp a -> Exp a
  EAbs  :: (Exp a -> Exp b) -> Exp (a -> [b])
  EApp  :: Exp (a -> [b]) -> Exp a -> Exp b
  EPlace :: a -> Exp a
    
dst :: Monad m => m a -> m b -> m (a, b)
dst ma mb = do a <- ma
               b <- mb
               return (a, b)

binopM :: (Functor m, Monad m) => (a -> b -> c) -> m a -> m b -> m c
binopM op ma mb = fmap (uncurry op) $ dst ma mb

eval :: Exp a -> [a]
eval e =
  case e of
    EInt  i -> return i
    EBool b -> return b
    EAdd e1 e2 -> binopM (+) (eval e1) (eval e2)
    EEq e1 e2 -> binopM (==) (eval e1) (eval e2)
    EIf e1 e2 e3 -> undefined
    EOr e1 e2 -> mplus (eval e1) (eval e2)
    EAbs e' -> return $ \x -> eval (e' (EPlace x))
    EApp f e' -> join $ binopM ($) (eval f) (eval e')
    EPlace a -> return a
-}

{- example
 * (\x. x + x) (1 or 2)
 * ((\f.f 0) or (\f.(f 1) + 1)) (\x.x)
-}
ex0, ex1 :: Exp m Int
ex0 = EApp (EAbs (\x -> EAdd x x)) (EOr (EInt 1) (EInt 2))
ex1 = EApp (EOr (EAbs (\f -> EApp f (EInt 0)))
                (EAbs (\f -> EAdd (EApp f (EInt 1)) (EInt 1))))
           (EAbs (\x -> x))
ex2 = EOr (EOr (EInt 1) (EInt 2)) (EInt 3)
-- ex3 = EOrWP (1/3) (EInt 0) (EInt 1)

data Exp m a where
  EInt  :: Int  -> Exp m Int
  EBool :: Bool -> Exp m Bool
  EAdd  :: Exp m Int -> Exp m Int -> Exp m Int
  EEq   :: (Eq a) => Exp m a -> Exp m a -> Exp m Bool
  EIf   :: Exp m Bool -> Exp m a -> Exp m a -> Exp m a
  EAbs  :: (Exp m a -> Exp m b) -> Exp m (a -> m b)
  EApp  :: Exp m (a -> m b) -> Exp m a -> Exp m b
  EPlace :: a -> Exp m a
  EOr   :: Exp m a -> Exp m a -> Exp m a             -- effect opr.
  -- EOrWP :: Double -> Exp m a -> Exp m a -> Exp m a   -- effect opr.
    
dst :: Monad m => m a -> m b -> m (a, b)
dst ma mb = do a <- ma
               b <- mb
               return (a, b)

mmap :: (Monad m) => (a -> b) -> (m a -> m b)
mmap f ma = (return . f) =<< ma

binopM :: Monad m => (a -> b -> c) -> m a -> m b -> m c
binopM op ma mb = mmap (uncurry op) $ dst ma mb

mif :: (Monad m) => m Bool -> m a -> m a -> m a
mif mb ma1 ma2 = mb >>= \b -> if b then ma1 else ma2

eval :: MonadPlus m => Exp m a -> m a
-- eval :: Exp IO a -> IO a
eval e =
  case e of
    EInt  i -> return i
    EBool b -> return b
    EAdd e1 e2 -> binopM (+) (eval e1) (eval e2)
    EEq e1 e2 -> binopM (==) (eval e1) (eval e2)
    EIf e1 e2 e3 -> mif (eval e1) (eval e2) (eval e3)
    EAbs e' -> return $ \x -> eval (e' (EPlace x))
    EApp f e' -> join $ binopM ($) (eval f) (eval e')
    EPlace a -> return a
    EOr e1 e2 -> mplus (eval e1) (eval e2)
    -- EOrWP r e1 e2 -> choiceWithProb r (eval e1) (eval e2)

-- choiceWithProb :: Double -> IO a -> IO a -> IO a
-- choiceWithProb r ma1 ma2 = mif (mmap (<=r) randomIO) ma1 ma2

instance MonadPlus IO where
  mzero = undefined
  mplus ma mb = mif randomIO ma mb
