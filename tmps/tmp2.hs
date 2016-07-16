-- import Prelude hiding (foldl, foldr)
import Control.Monad (join)

k :: a -> b -> a
k = \x y -> x

s :: (a -> b -> c) -> (a -> b) -> a -> c
s = \x y z -> x z (y z)

data MyList a = Nil
              | Cons a (MyList a)

{-
1 + a * (MyList a)  --->  1 + a * b
      ||                   ||  [eb, f]
      \/                   \/ 
    MyList a        --->    b
           myfoldr (eb, f)
-}

myfoldr :: (b, (a, b) -> b) -> MyList a -> b
myfoldr (eb, f) Nil         = eb
myfoldr (eb, f) (Cons ea l) = f (ea, result_l)
  where result_l = myfoldr (eb, f) l


hoge :: [a] -> b -> (a -> b -> Maybe b) -> Maybe b
hoge l b f = foldrM f b l

foldrMaybe :: (a -> b -> Maybe b) -> b -> [a] -> Maybe b
foldrMaybe f b []    = Just b
foldrMaybe f b (a:l) = f a =<< foldrMaybe f b l

foldrM :: Monad n => (a -> b -> n b) -> b -> [a] -> n b
foldrM = \f b l -> foldr (\a -> (f a =<<)) (return b) l

{-
two observations of "foldr"
- (a -> b -> b) -> b -> [a] -> b
 - a list of "a" is crashed to a "b" when knowing "how to crash in each step" and initial case.
- [a] -> (a -> b -> b) -> b -> b
 - initial state b is transformed to b' by the list of commands(a) and each step procedure,
 - in right-to-left order.
-}

-- foldl by foldr

fix :: (a -> a) -> a
fix f = let x = f x in x

subfact :: (Int -> Int) -> (Int -> Int)
subfact fact = \n ->  if n > 0
                      then n + (fact (n-1))
                      else 0

fact = fix subfact

{-
- "fix subfact" n         ("fix subfact" is not reduced)
- subfact "fix subfact" n
- if n > 0 then n + ("fix subfact" (n-1)) else 0
 - when n <= 0
   - 0
 - otherwise
   - n + "fix subfact" (n-1)
- ...
-}


data EvenList a = ENil
                | ECons a (OddList a)
data OddList a = OCons a (EvenList a)

data Reflexive a = C1 a
                 | C2 (Int -> Reflexive a)

data Reflexive' a = C1' a
                  | C2' (Reflexive a -> Int)
