{-# OPTIONS_GHC -XMultiParamTypeClasses -XFlexibleInstances -XFunctionalDependencies -XUndecidableInstances #-}

data Zero
  
data Succ a

class Add a b ab | a b -> ab, a ab -> b where
  add :: a -> b -> ab

instance Add Zero b b where
  add _ _ = undefined

instance (Add a b ab) => Add (Succ a) b (Succ ab) where
  add _ _ = undefined

z :: Zero
z = undefined

addone :: a -> Succ a
addone = undefined
