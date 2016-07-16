{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

import qualified Data.Foldable as Fold
import Control.Applicative
import Test.QuickCheck

data Tr a = Fork (Tr a) (Tr a)
          | Tip a
          deriving (Show, Eq, Functor, Fold.Foldable)
            
-- bird's replace minimum
repmin tr = tr'
  where (tr', m) = replace tr m
              
replace tr m =
  case tr of
    Tip x       -> let m' = min m x in (Tip m', x)
    Fork t1 t2  ->
      let (t1', m1) = replace t1 m
          (t2', m2) = replace t2 m
      in (Fork t1' t2', min m1 m2)
         
-- 2-pass replace minimum (naive implementation)
repmin' tr = fmap (const m) tr
  where m = Fold.foldr1 min tr
    
-- quickcheck
prop_repmin tr = repmin tr == repmin' tr
  where types = tr :: Tr Double
        
instance Arbitrary a => Arbitrary (Tr a) where
  arbitrary = oneof [ pure Tip <*> arbitrary
                    , pure Fork <*> arbitrary <*> arbitrary]

main = quickCheck prop_repmin