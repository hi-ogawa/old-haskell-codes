import Control.Applicative ((<*>), pure)
import Data.Functor ((<$>))
import Data.Function (fix)

import Test.QuickCheck
        
--myfoldr :: (a -> b -> b) -> b -> [a] -> b
--myfoldr f init ls = foldl (flip f) init (myrev ls)
--  
--myrev :: [a] -> [a]
--myrev ls = foldl (\acc e -> e:acc) [] ls

prop_foldl i l = myfoldl f i l == foldl f i l
  where types = (f :: Int -> Int -> Int,
                 i :: Int,
                 l :: [Int])
        f = \x -> \y -> x + y - x * y

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f init ls = 
  ($init) $ foldr (\e cont -> (\res -> cont (f res e))) id ls
  
myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f init ls = 
  ($init) $ foldl (\cont e -> (\res -> cont (f e res))) id ls

-- foldl (+) 0 [1, 2, 3]
-- (((0 + 1) + 2) + 3)

-- foldrで書く
-- <g1> (\res -> res + 3)
-- <g2> (\res -> g1 (res + 2))
-- <g3> (\res -> g2 (res + 1))

addc :: ((a -> a) -> (a -> a)) -> 
        ((a -> a) -> (a -> a)) -> 
        ((a -> a) -> (a -> a))
addc n m = \s -> \z -> ((n . m $ s) $ z)

-- myId x = x
-- myId x = let _ = foo in x
-- foo = (myId 1, myId "a")
-- foo = (myId True, myId "a")


ex0 = fix $ (<$>)<$>(:)<*>((<$>((:[])<$>)) (=<<)<$>(*)<$>(*2)) $ 1

{-
(<$>((:[])<$>)) (=<<)
(\f -> fmap f (\x -> fmap (:[]) x)) (=<<)
fmap (=<<) (\x -> fmap (:[]) x)
\x -> (=<<) ( (\x -> fmap (:[]) x) x)
\x -> (=<<) (fmap (:[]) x)

.. <$>(*)<$>(*2)
\y -> (\z -> (\x -> (=<<) (fmap (:[]) x)) (\h -> z*h)) (y * 2)
\y -> (\z -> (=<<) (fmap (:[]) (\h -> z*h))) (y * 2)
\y -> (=<<) (fmap (:[]) (\h -> y*2*h))
\y -> (\m -> ((fmap (:[]) (\h -> y*2*h)) (=<<) m))
\y -> (\m -> ((\h -> ((y*2*h):[]))) =<< m)
  
(<$>)<$>(:)<*> ..
(fmap (fmap) (:)) <*> ..
(\x -> fmap (x:)) <*> (\y -> (\m -> ((\h -> ((y*2*h):[]))) =<< m))
\r -> fmap (r:) $ \m -> ((\h -> ((r*2*h):[]))) =<< m
\r -> \m -> (r:) (m >>= ((\h -> ((r*2*h):[]))))
\r -> \m -> (r:) $ concatMap (\h -> ((r*2*h):[])) m
\r -> \m -> r : (map (\t -> r*2*t) m )

fix $ .. $ 1
fix $ \m -> 1 : (map (2*) m)


fix f = f (fix f)
-}