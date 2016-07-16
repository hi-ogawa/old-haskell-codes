{-# LANGUAGE Rank2Types #-}

-- church numeral can be defined elegantly by using higher rank (rank-2) type.
-- "subtraction" (predecessor) is not defined in simple type case.
-- でもだめだったぽい...
-- newtypeでちゃんとくるめばいける!! (ChurchNumeral2.hs)

type ChInt = forall a. a -> (a -> a) -> a

chintToInt :: ChInt -> Int
chintToInt c = c 0 (+1)

intToChint :: Int -> ChInt
intToChint 0 = \z -> \s -> z
intToChint n = \z -> \s -> s (intToChint (n-1) z s)

ch0, ch1 :: ChInt
ch0 = intToChint 0
ch1 = intToChint 1

chPlus :: ChInt -> ChInt -> ChInt
chPlus c1 c2 = \z -> \s -> c1 (c2 z s) s

-- chPred :: ChInt -> ChInt
chPred :: (forall a. a -> (a -> a) -> a) -> (forall a. a -> (a -> a) -> a)
chPred c = \z -> \s -> car (c (cons z z) (\t -> cons (cdr t) (s (car t))))

chPred' c = \z -> \s -> c (\_ -> z) (\g -> \h -> h (g s)) (\u -> u)


{-
- (\_ -> z)
- (\g -> \h -> h (g s)) (\_ -> z)
   == \h -> h ((\_ -> z) s)
   == \h -> h z
- (\g -> \h -> h (g s)) (\h -> h z)
   == \h -> h ((\h' -> h' z) s)
   == \h -> h (s z)
- (\g -> \h -> h (g s)) ((\g -> \h -> h (g s)) l)
  (\g -> \h -> h (g s)) (\h -> h (l s))
  (\h -> h ((\h -> h (l s)) s))
-}

-- chSub :: ChInt -> ChInt -> ChInt
-- -- chSub c1 c2 = \z -> \s -> (c2 c1 chPred) z s
-- chSub c1 c2 = c2 c1 chPred


type ChBool = forall a. a -> a -> a

true, false :: ChBool
true = \x -> \y -> x
false = \x -> \y -> y

type ChTuple a = (a -> a -> a) -> a

cons :: a -> a -> ChTuple a
cons ca cd = \f -> f ca cd
  
car :: ChTuple a -> a
car t = t true

cdr :: ChTuple a -> a
cdr t = t false

--------------------------------------------------------
-- famous example not typable in simple typed lambda. --
--------------------------------------------------------

ex = (id True, id 0)                       -- ok
-- ex' = (\f -> (f True, f (0 :: Int))) id -- bad
ex' = g id                                 -- ok!!
  where g :: (forall a. a -> a) -> (Bool, Int)
        g = \f -> (f True, f 0)
