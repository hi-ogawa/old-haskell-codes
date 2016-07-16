{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module InitialFinalTrace where
  
import Data.Char (chr)
import Control.Monad

-------------------------------------------------
-- initial algebra and its inductive extention --
-------------------------------------------------

data Fix f = FixC (f (Fix f))

initAlg :: f (Fix f) -> Fix f
initAlg t = FixC t

ind :: Functor f => (f x -> x) -> (Fix f -> x)
ind a (FixC ft) = a . (fmap (ind a)) $ ft


-- ex. list functor (stream functor?)

data ListF a x = Halt
               | Next a x deriving (Functor, Show)

type ListFC = ListF Char

type MyString = Fix ListFC

my_length_a :: ListFC Int -> Int
my_length_a (Halt)     = 0 
my_length_a (Next c i) = 1 + i

my_length :: MyString -> Int
my_length = ind my_length_a

ls0 :: MyString
ls0 = FixC (Next 'a' (FixC (Next 'b' (FixC (Next 'c' (FixC Halt))))))

test = my_length ls0

-- but, we can also define infinite data in MyString as usual in Haskell
ls_a = FixC (Next 'a' ls_a)


---------------------------------------------------
-- final coalgebra and its coinductive extention --
---------------------------------------------------

-- in fact, we can use "Fix" as a carrier of final coalgebra.
-- and we can define the final coalgebra by unfolding the outer constructor

finalC :: (Fix f) -> f (Fix f)
finalC (FixC ft) = ft

beh :: Functor f => (x -> f x) -> (x -> Fix f)
beh c = invFinalC . fmap (beh c) . c
  where invFinalC = initAlg :: f (Fix f) -> Fix f
  
-- ex. stream (possibly finite)

nat2chr :: Int -> Char
nat2chr n | 0 <= n && n <= 9 = chr (n + 48)
          | otherwise        = undefined

genlist :: Int -> ListFC Int
genlist 0 = Halt
genlist n = Next (nat2chr n) (n-1)

ls1 :: MyString
ls1 = beh genlist 5

ls_A :: MyString
ls_A = beh (\x -> Next 'a' x) ()

-- the show method for the carrier of initial, final coalgebra
class Functor f => ShowF f where
  showf :: Show x => f x -> String

instance Show a => ShowF (ListF a) where
  showf = show

instance ShowF f => Show (Fix f) where
  show (FixC t) = "(FixC (" ++ (showf t) ++ "))"



---------------------
-- trace semantics --
---------------------

class (Functor m, Monad m, Functor f) => Dist m f where
  dist :: (f (m a)) -> (m (f a))
  klLift :: (x -> m y) -> (f x -> m (f y))


trace :: Dist m f => (x -> m (f x)) -> (x -> m (Fix f))
trace c = (return . initAlg)  <=<  (dist . fmap (trace c))  <=<  c
         

-- ex. accepted language by nondet. automata

instance Dist [] ListFC where
  dist Halt         = [Halt]
  dist (Next ch ls) = do x <- ls
                         return (Next ch x)
  klLift = undefined


lang0 ::  [MyString]
lang0 = trace c 5
  where c :: Int -> [ListFC Int]
        c 0 = [Halt]
        c n = [Next (nat2chr n) (n - 1)]

lang1 :: [MyString]
lang1 = trace c 4
  where c :: Int -> [ListFC Int]
        c n | n < 0     = [Halt]
            | otherwise = [Next (nat2chr n) (n - 1), Next (nat2chr n) (n - 2)]
