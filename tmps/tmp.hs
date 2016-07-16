import Control.Monad

-- powerset :: MonadPlus m => [a] -> m [a]
-- powerset []     = return []
-- powerset (x:xs) = do ps' <- powerset xs
--                      return (x:ps') `mplus` return ps'
                     
-- nats = 0 : map (+1) nats


-- newtype SM e s a = SM {unSM :: (s -> Either e (a, s))}

-- -- instance Functor (SM e s) where
-- --   fmap f (Left er) = undefined
-- --   fmap f (Right g) = undefined
  
-- instance Monad (SM e s) where
--   return a = SM $ Right (\s -> (a, s))
--   (SM (Left er)) >>= f = SM $ Left er
--   (SM (Right g)) >>= f = SM $ Right (\s -> let (a, s') = g s in
--                                        case f a of
--                                          (SM (Left  er)) -> error "hoge"
--                                          (SM (Right f')) -> f' s')

-- instance Monad [] where
--   return = \x -> [x]
--   join   = \lls -> concat lls

comp' :: (Functor m, Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
comp' f g = join . (fmap g) . f

bind :: (Monad m, Functor m) => m a -> (a -> m b) -> m b
bind m f = comp' (const m) f $ ()



-- recursive definition by the initiality (ref. jacobs-coalgebra. chap2)--
-- h :: Int -> a
-- h 0 = f
--   where f = undefined
-- h n = g (h (n-1))
--   where g = undefined
  
-- l :: Int -> a
-- l 0 = f
--   where f = undefined
-- l n = g (n-1) (h (n-1))
--   where g = undefined
        
f :: p -> x
f = undefined
g :: x -> p -> x
g = undefined

e :: Int -> p -> x
e 0 p = f p
e n p = g (e (n-1) p) p


evens xs = head xs : (evens (tail xs))
odds xs = evens (tail xs)
