{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
import Control.Exception.Base (assert)

-- FunctionalDependencies, 

class Collect ce e where -- | ce -> e where
  empty :: ce
  
instance Eq a => Collect [a] a where  
  empty = undefined
  
instance Collect Bool Int where  
  empty = undefined

-- :t head empty
-- head empty :: Collect [a] e => a
-- :t [head empty, head empty]
-- [head empty, head empty] :: (Collect [t] e1, Collect [t] e) => [t]
  
main = assert False (return 1)