{-# LANGUAGE Rank2Types #-}

newtype Chint = Ch { unCh :: (forall a. a -> (a -> a) -> a) }

ch0 :: Chint
ch0 = Ch $ \z f -> z

fromChint :: Chint -> Int
fromChint c = (unCh c) 0 (+1)
  
toChint :: Int -> Chint
toChint 0 = Ch $ \z f -> z
toChint n = Ch $ \z f -> f ((unCh (toChint (n - 1))) z f)

instance Show Chint where
  show = show . fromChint


chPlus :: Chint -> Chint -> Chint
chPlus c0 c1 = Ch $ \z f -> (unCh c0) ((unCh c1) z f) f


chPred :: Chint -> Chint
chPred c = Ch $ \z f -> car $ (unCh c) (cons z z) (\t -> cons (cdr t) (f (cdr t)))
  where cons a b = \f -> f a b
        car t = t (\a b -> a)
        cdr t = t (\a b -> b)

chSub :: Chint -> Chint -> Chint
chSub c0 c1 = (unCh c1) c0 chPred

{-
*Main> chSub (toChint 25) (toChint 14)
11
-}


