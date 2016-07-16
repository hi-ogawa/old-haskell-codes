import Criterion.Main
import Data.List (sort)
import System.Random
import qualified Data.DList as DL

class ListLike f where
  nil :: f a
  cons :: a -> f a -> f a
  app :: f a -> f a -> f a
  toList :: f a -> [a]
  fromList :: [a] -> f a
  fromSingleton :: a -> f a
  
newtype L a = L ([a] -> [a])

{-
[1,2,3,4] ++ [5,6,7,8]
(1: (2: (3: (4: [5,6,7,8]))))

[1,2,3,4]
\l -> (1: (2: (3: 4:l)))

([1,2,3] ++ [4,5,6]) ++ [7,8,9]
(\l0 -> (\l1 -> (\l2 -> [1,2,3] ++ l2) ((\l3 -> [4,5,6] ++ l3) l1)) ((\l4 -> [7,8,9] ++ l4) l0))

(([1] ++ [2]) ++ [3]) ++ [4]
(\l -> (\l -> (\l -> (\l -> [1]++l) ((\l -> [2]+l) l)) ((\l -> [3]++l) l)) ((\l -> [4]++l) l))

-}

instance ListLike L where
  nil               = L $ \l -> []
  cons x (L f)      = L $ \l -> x:(f l)
  app (L f1) (L f2) = L $ \l -> f1 (f2 l)
  toList (L f)      = f []
  fromList ls       = L $ \l -> ls++l
  fromSingleton x   = L $ \l -> x:l

myRevD :: [a] -> [a]
myRevD ls = toList $ foldr (\x acc -> acc `app` fromSingleton x) n ls
  where n = nil :: L a

myRev :: [a] -> [a]
myRev ls = foldr (\x acc -> acc ++ [x]) [] ls

main = 
  do g <- getStdGen
     defaultMain [
       bgroup "reverse" $ benchs_list 100 ++ benchs_dlist 100
       ]

benchs_list :: Int -> [Benchmark]
benchs_list  n = map (\n -> bench ("list-"  ++ show n) $ nf myRev [1..n]) [0,10..n]

benchs_dlist :: Int -> [Benchmark]
benchs_dlist n = map (\n -> bench ("dlist-" ++ show n) $ nf myRevD [1..n]) [0,10..n]