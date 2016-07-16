import Test.QuickCheck
import Data.List (sort)

insert :: Ord a => a -> [a] -> [a]
insert x []                = [x]
insert x (hd:tl) | x < hd  = x : hd : tl
                 | x == hd = x : hd : tl
                 | hd < x   = hd : insert x tl


isort :: Ord a => [a] -> [a]
isort = foldr insert []

prop_isort :: [Int] -> Bool
prop_isort l = sort l == isort l

main = quickCheck prop_isort


data Nat = O
         | S Nat deriving (Show, Eq)

natToInt :: Nat -> Int
natToInt O = 0
natToInt (S n) = 1 + natToInt n

-- le :: Nat -> Nat -> Bool
-- le 

decide_le :: Nat -> Nat -> Either () ()
decide_le m O      = Left ()
decide_le m (S n') =
  case decide_le m n' of
    Left  () ->          --  m >= n' 
    Right () -> Right () --  m <= n' implies m <= (S n')
