import Data.Array
import Data.Array.ST -- overloaded Data.Array.MArray
import Control.Monad (forM_, forM)
import qualified Control.Monad.ST.Trans as S
import Control.Monad.Error
import Control.Monad.State 
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Mp
import Debug.Trace (trace)


-- 隣接行列 --
type Graph1 attr = Array (Int, Int) attr
getSize1 :: Graph1 a -> Int
getSize1 gr = let (_, (n,_)) = bounds gr in n

-- 隣接リスト --
type Graph2 attr = Array Int [attr]
getSize2 :: Graph2 a -> Int
getSize2 gr = let (_, n) = bounds gr in n

-- 探索 --

-- 深さ優先 (隣接行列)
solve1 :: Graph1 Int -> Either String (Mp.Map Int Color)
solve1 gr = 
  flip execStateT (Mp.fromList $ map (\i -> (i, Undef)) [1..n]) $ -- Stateだけ取り出す
  mapM (\i -> do Just c <- gets (Mp.lookup i)
                 case c of
                   Undef        -> do modify (Mp.insert i Red)
                                      dfs1 gr i Red
                   _            -> return ()
       ) [1..n]
  where (_, (n, _)) = bounds gr

dfs1 :: Graph1 Int -> Int -> Color -> StateT (Mp.Map Int Color) (Either String) ()
dfs1 gr node c = mapM_ (\i -> do Just c' <- gets (Mp.lookup i)
                                 case c' of
                                   Undef           -> do modify (Mp.insert i (rev c))
                                                         dfs1 gr i (rev c)
                                   _ | c' /= rev c -> do throwError ("can't draw two colors")
                                   _               -> return ()
                       ) rinsetu
  where rinsetu = filter (\i -> gr!(node, i) == 1) [1..n]
        (_, (n, _))  = bounds gr
        
-- 深さ優先(2色問題) (隣接リストで)
data Color = Undef | Red | Blue deriving (Show, Eq)
type Graph2' = Array Int (Color, [Int])

solve2 :: Graph2 Int -> Either String Graph2'
solve2 gr = 
  S.runSTArray 
  $ do grST <- S.thawSTArray gr' 
       forM_ [1..n] (\i -> do (c, _) <- S.readSTArray grST i      -- 連結でない場合もあるので、全てのノードをチェック
                              case c of
                                Undef     -> dfs2 grST i Red
                                _         -> return ()
                    )
       return grST
  where gr'    = fmap (\st -> (Undef, st)) gr
        (_, n) = bounds gr
        
dfs2 :: S.STArray s Int (Color, [Int]) -> Int -> Color -> S.STT s (Either String) ()
dfs2 grST i c = do (_, is) <- S.readSTArray grST i
                   S.writeSTArray grST i (c, is)
                   forM_ is (\i' -> do (c', _) <- S.readSTArray grST i'
                                       case c' of
                                         Undef   -> dfs2 grST i' (rev c)
                                         _       | c' == c -> throwError "can't draw two colors!"
                                         _       -> return ()
                            )
rev Red = Blue
rev Blue= Red


-- 幅優先 (隣接行列)        
solve3 :: Graph1 Int -> Mp.Map Int Color
solve3 gr = 
  flip execState (Mp.insert 1 Red iniMap) $
  bfs3 gr [(1, Red)]
  where n = getSize1 gr
        iniMap = Mp.fromList $ map (\i -> (i, Undef)) [1..n]

bfs3 :: Graph1 Int -> [(Int, Color)] -> State (Mp.Map Int Color) ()
bfs3 gr []             = return ()
bfs3 gr ((node,c):que) = 
  do let rinsetu = filter (\i -> gr!(node,i) == 1) [1..n]
     trace (show node) (return ())
     nque <- concat `fmap` forM rinsetu (\i -> do Just c' <- gets (Mp.lookup i)
                                                  case c' of
                                                    Undef      -> do modify (Mp.insert i (rev c)); return [(i, rev c)]
                                                    _ | c' == c-> error "owari"
                                                    _          -> return []
                                        )
     bfs3 gr (que++nque)
  where n = getSize1 gr
        

-- 反復深化 (隣接リスト)
solve4 :: Graph2 Int -> [Int]
solve4 gr = flip execState [] $ forM [0..n] (\d -> iddfs gr d 1)
  where n = getSize2 gr
        
iddfs :: Graph2 Int -> Int -> Int -> State [Int] ()
iddfs gr 0     node = do b <- gets (elem node) 
                         if b then return () else modify (++[node])
iddfs gr depth node = forM_ rin (iddfs gr (depth-1))
  where rin = gr!node

-- 一列化 (深さ優先探索 => 帰りがけにノードを出力)
-- solve5 :: Graph2 Int -> [Int]
-- solve5 gr = forM [1..n] (\i -> dfs5 gr i)
--   where n = getSize gr

-- dfs5 gr node = do forM_ rin (\i -> do gets 
--                             )
--   where rin = gr!node

-- parsing --
handleInput :: IO (Int, Int, [(Int, Int)])
handleInput = do [n, e] <- (map read . words) `fmap` getLine
                 ses <- forM [1..e] (\_ -> ((\ [st, en] -> (st, en)) . map read . words) `fmap` getLine)
                 return (n, e, ses)

toGraph1 :: (Int, Int, [(Int, Int)]) -> Graph1 Int
toGraph1 (n, e, ses) = 
  runSTArray $ do arr <- newArray ((1,1), (n,n)) 0
                  forM_ ses (\ ii@(st, en) -> writeArray arr ii 1 >> writeArray arr (en, st) 1)
                  return arr
                  
toGraph2 :: (Int, Int, [(Int, Int)]) -> Graph2 Int
toGraph2 (n, e, ses) =
  runSTArray $ do arr <- newArray (1, n) []
                  forM_ ses (updateArray arr)
                  return arr
  where updateArray arr (st, en) = do [s1, s2] <- mapM (readArray arr) [st, en]
                                      writeArray arr st (en:s1)
                                      writeArray arr en (st:s2)

-- show --
showGraph1 :: Graph1 Int -> String
showGraph1 arr = concatMap ((++"\n") . concatMap (\e -> take 5 $ show e ++ "     ")) ess
  where (_, (n, _)) = bounds arr
        ess         = wakeru n . elems $ arr
        
showGraph2 :: Graph2 Int -> String                  
showGraph2 arr = concatMap (\ (i, si) -> show i ++" -> "++show si++"\n") . assocs $ arr
  where (_, n) = bounds arr

showGraph2' :: Graph2' -> String                  
showGraph2' arr = concatMap (\ (i, (c, si)) -> show i ++" -> "++show c++": "++show si++"\n") . assocs $ arr
  where (_, n) = bounds arr

wakeru :: Int -> [a] -> [[a]]
wakeru n [] = []
wakeru n as = as1:(wakeru n as2)
  where (as1, as2) = splitAt n as
        

testGr1 :: Graph1 Int
testGr1 = toGraph1 test

testGr2 :: Graph2 Int
testGr2 = toGraph2 test

test = (n, e, ses)
  where x:xs = lines . unsafePerformIO . readFile $ "test.txt"
        [n,e]= map read . words $ x
        ses  = map ((\[a,b] -> (a,b)) . map read . words) xs
        
{- input specification

n e     -- <num of node> <num of edge>
st en   -- <start node> and <end node> of edge1
st en   -- ..
..
..

-- sample
6 8
6 1
6 5
1 5
1 4
1 2
2 3
3 4
4 5

-}
