{-# OPTIONS_GHC -XGADTs -XExistentialQuantification -XRankNTypes -XImpredicativeTypes -XMultiParamTypeClasses -XFlexibleInstances -XFunctionalDependencies -XUndecidableInstances #-}

-- from https://en.wikibooks.org/wiki/Haskell/GADT --

-- data Exp = EInt Int
--          | EBool Bool
--          | EAdd Exp Exp
--          | EEq  Exp Exp
--          deriving (Show, Eq)



-- data PrimExp = EInt Int
--              | EBool Bool
--              | EAdd PrimExp PrimExp
--              | EEq PrimExp PrimExp
--              deriving Show
                      
-- data Exp a = Exp PrimExp
--              deriving Show

-- add :: Exp Int -> Exp Int -> Exp Int
-- add (Exp p1) (Exp p2) = Exp (EAdd p1 p2)

-- makeInt :: Int -> Exp Int
-- makeInt i = Exp (EInt i)

-- data Exp a where
--   EInt  :: Int -> Exp Int
--   EBool :: Bool -> Exp Bool
--   EAdd  :: Exp Int -> Exp Int -> Exp Int

data Empty
data NonEmpty

data SafeList a b where
  Nil  :: SafeList a Empty
  Cons :: a -> SafeList a b -> SafeList a NonEmpty

safeHead :: SafeList a NonEmpty -> a
safeHead (Cons e _) = e

-- type checking fails
-- silly False = Nil
-- silly True  = Cons Nil Nil



-- https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types
data ShowBox = forall s. Show s => SB s

instance Show ShowBox where
   show (SB s) = show s

-- type Hoge = [forall a. a]
type Hoge = [forall a. Num a => a]

-- num = 1 :: Int

-- hoge :: Hoge
-- hoge :: [forall a. Num a => a]
-- hoge :: [forall a. a]
-- hoge = [undefined]


data T = forall a. MkT a

hoge = [MkT True, MkT 'h', MkT "st"]

-- interesting example is in "Lazy Functional State threads", which
-- is an article about ST Monad in Haskell


-- functional dependencies

data Vector a = V a a
data Matrix a = M (Vector a) (Vector a)

instance Show a => Show (Vector a) where
  show (V e1 e2) = show e1 ++ " - " ++ show e2

instance Show a => Show (Matrix a) where
  show (M v1 v2) = show v1 ++ " - " ++ show v2


instance Num a => Num (Vector a) where
  (V e1 e2) + (V e1' e2') = V (e1 + e1') (e2 + e2')
  _ * _ = undefined
  abs _ = undefined
  signum _ = undefined
  fromInteger _ = undefined
  
instance Num a => Num (Matrix a) where
  (M v1 v2) + (M v1' v2') = M (v1 + v1') (v2 + v2')
  _ * _ = undefined
  abs _ = undefined
  signum _ = undefined
  fromInteger _ = undefined

class Mult a b c | a b -> c where
  (|*|) :: a -> b -> c

-- instance Mult (Matrix a) (Matrix b) (Matrix a) where
--   (M _ _) * (M _ _) = undefined

instance Num a => Mult a a a where
  e1 |*| e2 = e1 * e2
  
instance Mult a b c => Mult (Vector a) b (Vector c) where
  (V a1 a2) |*| b = V (a1 |*| b) (a2 |*| b)

instance Mult a b c => Mult (Matrix a) b (Matrix c) where
  (M v1 v2) |*| b = M (v1 |*| b) (v2 |*| b)

-- instance Mult a b c => Mult (Matrix a) b Int where
--   (M v1 v2) |*| b = 0

i1 :: Int
i1 = 5

vec1, vec2 :: Vector Int 
vec1 = V 1 2
vec2 = V 3 4

mat :: Matrix Int
mat = M vec1 vec2
