class ContraFunctor f where
  cfmap :: (a -> b) -> (f b -> f a)
  
--type PowerSet a = a -> Bool
-- newtype PowerSet a = PowerSet {un :: a -> Bool}

-- instance ContraFunctor PowerSet where
-- --  cfmap :: (a -> b) -> PowerSet b -> PowerSet a
--   cfmap f = \bs -> (PowerSet (\a -> (un bs) (f a)))
  
-- ret :: a -> PowerSet (PowerSet a)
-- ret = c

-- class (Functor f, Functor g) => Adj f g where
--   adj :: (Functor f, Functor g) => (a -> g b) -> (f a -> b)

type PowerSet a = a -> Bool

bijection :: (a -> PowerSet b) -> (b -> PowerSet a)
bijection f = \y -> (\x -> (f x) y)