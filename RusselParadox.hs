{-# LANGUAGE ExistentialQuantification #-}

data U = forall a. Set a (a -> Bool) (a -> U)

