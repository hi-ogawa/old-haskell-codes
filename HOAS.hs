{-# LANGUAGE GADTs #-}

-- motivation: represent the language with variable binding
-- cf. http://en.wikipedia.org/wiki/Higher-order_abstract_syntax
-- cf. fl-2012-10.pdf

{- ordinary abstract syntax -}
-- Here, the type "Prog" is an algebraic data type, so "Prog" represents nothing but a tree structure, which does not accomodate any information about variable binding relation. So, we need to inspect the structure after construct it to find binding relation with it correctly.
-- (the advantage is that this trivial representation can be used in any language.)

type Id = String
data Prog = PInt Int
          | PPlus Prog Prog
          | PVar Id
          | PAbs Id Prog
          | PApp Prog Prog

p0 :: Prog
p0 = PApp (PAbs "x" (PPlus (PVar "x") (PInt 1))) (PInt 2)


{- higher order abstract syntax -}
-- But, higher order abstract syntax can be realized by GADTs. Here, "HProg" represents the type which has a binding relation encoded in the host language (Haskell).

data HProg a where
  HPInt  :: Int -> HProg Int
  HPPlus :: HProg Int -> HProg Int -> HProg Int
  HPAbs  :: (HProg a -> HProg a) -> HProg (a -> a)
  HPApp  :: HProg (a -> b) -> HProg a -> HProg b

p1 :: HProg Int
p1 = HPApp (HPAbs (\x -> HPPlus x (HPInt 1))) (HPInt 2)

-- not only a binding relation, but also type information is accomodated in the type "HProg".
