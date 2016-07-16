import Data.List(permutations, find)
import Data.Maybe(fromJust)
import Data.Ix(inRange)

main :: IO ()
main = getLine >>= printQueen.solve.read

printQueen :: [Int] -> IO ()
printQueen is = mapM_ (\i -> do putStrLn ((take (i-1) $ repeat '*')
                                          ++ "@"
                                          ++ (take (n-i) $ repeat '*'))
                      ) is
  where n = length is
  
solve :: Int -> [Int]
solve size = fromJust $ find (check size) $ permutations [1..size]

check :: Int -> [Int] -> Bool
check size is = myFold size xys
  where xys = zip [1..] is -- 座標表示
        
myFold :: Int -> [(Int, Int)] -> Bool
myFold size []       = True
myFold size (xy:xys) = diagCheck size xy xys && myFold size xys

diagCheck size xy xys = not $ or $ map (\xy -> elem xy outs) xys
  where outs = genOuts size xy
        
genOuts size xy = mms ++ mps ++ pms ++ pps
  where mms = takeWhile (inRange ((1,1), (size,size))) $ iterate (\(x,y) -> (x-1,y-1)) xy
        mps = takeWhile (inRange ((1,1), (size,size))) $ iterate (\(x,y) -> (x-1,y+1)) xy
        pms = takeWhile (inRange ((1,1), (size,size))) $ iterate (\(x,y) -> (x+1,y-1)) xy
        pps = takeWhile (inRange ((1,1), (size,size))) $ iterate (\(x,y) -> (x+1,y+1)) xy


