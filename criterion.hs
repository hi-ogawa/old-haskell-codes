import Criterion.Main
import Data.List (sort)
import System.Random
import qualified Data.DList as DL

main = do
     g <- getStdGen
     defaultMain [
--      bgroup "sort" [ bench "ascending"   $ nf sort generator
--                    , bench "descending"  $ nf sort (reverse (generator))
--                    , bench "randoms"     $ nf sort $ take (10^4) $ (randoms g::[Int])
--                    ]
--      , 
       bgroup "append" [ bench "list" $ nf rep_append [1..10]
                       , bench "dlist"$ nf rep_appendD [1..10]
                       ]
       ]

generator :: [Int]
generator = [0..10^4]

-- concatがちゃんと後ろからappendしてるから変わらない
rep_append :: [Int] -> [Int]
rep_append = concat . replicate (10^4)

rep_appendD :: [Int] -> [Int]
rep_appendD = DL.toList . DL.concat . DL.toList . DL.replicate (10^4) . DL.fromList
