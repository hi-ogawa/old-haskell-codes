module MySqrt (mySqrt) where

import Data.List (find)

eps = 1.0e-10

mySqrt :: Double -> Maybe Double
mySqrt i = do
  (a, dif) <- find (\(a, dif) -> dif < eps) $ (\ls -> zipWith (\a b -> (a, abs (a-b))) ls $ tail ls) $ iterate (\x -> (i+x*x)/(2*x)) $ i / 2
  return a