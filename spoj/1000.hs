-- import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BS
import Data.List (splitAt)
import Control.Monad (when, forM)
import qualified Data.Set as St

main = do ls <- BS.lines `fmap` BS.getContents  -- all IO in one go
          main' ls
          
main' :: [BS.ByteString] -> IO ()
main' (l:ls) = if n == 0 
               then return ()
               else do putStrLn (if main'' n ls' then "good" else "wrong")
                       main' nextls
  where Just(n, _) = BS.readInt l
        (ls', nextls) = splitAt (n - 1) ls
        
myReadInt :: BS.ByteString -> Int        
myReadInt bs = i
  where Just (i, _) = BS.readInt bs
                    
main'' :: Int -> [BS.ByteString] -> Bool
main'' n ls = and $ map solve areas
  where pss = map (St.fromList . listToPairs . (map myReadInt) . BS.words) ls
        lastArea = St.difference (St.fromList [(x,y) | x <- [1..n], y <- [1..n]]) (St.unions pss)
        areas    = pss ++ [lastArea]
        
listToPairs :: [Int] -> [(Int, Int)]
listToPairs []        = []
listToPairs (x:y:xs)  = (x,y):(listToPairs xs)

solve :: St.Set (Int, Int) -> Bool
solve area = solve' St.empty area
  
solve' :: St.Set (Int, Int) -> St.Set (Int, Int) -> Bool
solve' area1 area2 | St.null area2 = True
solve' area1 area2 | St.null area1 = solve' (St.singleton min) area2'
  where (min, area2') = St.deleteFindMin area2
solve' area1 area2  = if St.null area1'
                      then False
                      else solve' (St.union area1 area1') area2'
  where oks = (\ar -> St.difference ar area1)
              $ St.fold (\(x,y) st -> St.union st $ St.fromList [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
                        ) St.empty area1
        (area1', area2') = St.partition (\e -> St.member e oks) area2
