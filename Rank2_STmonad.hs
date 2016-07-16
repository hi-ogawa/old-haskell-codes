{-# LANGUAGE Rank2Types #-}

import Control.Monad.ST
import Data.STRef

{- ∀∃

runST :: ∀a. ((∀s. (ST s a)) -> a)

newSTRef :: ∀a. (a -> (∀s. (ST s (STRef s a))))

readSTRef :: ∀a. (∀s. (STRef s a -> ST s a))

writeSTRef :: ∀a. (∀s. (STref s a -> a -> ST s () ))

bindST :: ∀a. ∀b. ((a -> ∀s. ST s b) -> ((∀s. (ST s a)) -> (∀s. (ST s b))))
bindST :: ∀a. ∀b. ∀s. ((a -> ST s b) -> (ST s a -> ST s b))

returnST :: ∀a. (a -> ∀s. (ST s a))

-}

-- based on the newtype ST', above primitive definitions can be rewritten??
newtype ST' a = ST' {unST' :: forall s. ST s a}

runST' :: ST' a -> a
runST' = undefined

newSTRef' :: a -> ST' a
newSTRef' = undefined

bindST :: ST s a -> (a -> ST s b) -> ST s b
bindST = (>>=)

ex0 = (newSTRef True) `bindST` (\v -> readSTRef v)

ex1 :: forall s. ST s (Bool, Bool)
ex1 = do var <- newSTRef True
         val0 <- readSTRef var
         writeSTRef var False
         val1 <- readSTRef var
         return (val0, val1)


-- this code should not be allowed (should be untypable)
-- v = runST (newSTRef True)
-- ex2 = runST (readSTRef v)
