import Control.Monad.Reader
import Control.Monad(replicateM)
import Data.List(nub)
import Data.Array.IO
import Data.Array
import Data.IORef

type Pack  = Array (Int,Int) Int
type Board = IOArray (Int,Int) Int

main = do [w, h, t, s, n] <- fmap ((map read).words) getLine
          packs <- readPacks n []
          start packs
          forM_ [0..100] (\_ -> putStrLn (" 0"))
          
readPacks :: Int -> [Pack] -> IO [Pack]
readPacks 0 acc = return acc
readPacks n acc = do ls <- replicateM 4 getLine
                     _  <- getLine  -- read "END"
                     readPacks (n-1) (acc ++ [listArray ((1,1), (4,4))
                                              $ ((map read).words.unlines.reverse) ls])
                         
start :: [Pack] -> IO ()
start packs = do initBoard <- newArray ((1,1), (20, 10)) 0 -- boardは参照
                 initScore <- newIORef 0
                 loop initBoard initScore packs


loop :: Board -> IORef Int -> [Pack] -> IO ()
loop _     _     []    = return ()
loop board score packs = do (pos, angle) <- getNext board packs
                            printPack (head packs)                    -- debug pack
                            printCommand (pos, angle)                 -- output
                            updateBoard board (pos,angle) (head packs)
                            printBoard board                          -- debug board
                            chainCheck board score 0 0
                            printBoard board                          -- debug board
                            end <- endCheck board
                            (if end
                             then return ()
                             else loop board score (tail packs))
                        
-- 次にどこに落とすか選択
getNext :: Board -> [Pack] -> IO (Int, Int)
getNext board packs = return (3,0)
  
-- boardの更新
updateBoard :: Board -> (Int, Int) -> Pack -> IO ()
updateBoard board (pos, angle) pack =
  forM_ drops (\(col, num) -> dropBoard board (col, num))
  where pack' = rotatePack pack angle
        drops = getDropPack pack' pos
        
dropBoard :: Board -> (Int, Int) -> IO ()
dropBoard board (col, num) = 
  do blocks <- mapM (readArray board) (zip [1..16] [col,col..])
     let row = 1 + (length $ takeWhile (0/=) blocks)
     writeArray board (row, col) num
     
-- 連鎖のチェック, boardの更新
chainCheck :: Board -> IORef Int -> Int -> Int -> IO ()
chainCheck board score ch co =
  do iis <- fmap (concatMap (\(ii, e) -> if e == 0 then [] else [ii])) $ getAssocs board
     let ls = concatMap (\ii -> getLs ii iis) iis
     tenLs <- filterM (\line -> do es <- mapM (readArray board) line
                                   return (sum es == 10)
                      ) ls
     if tenLs /= []
       then do putStrLn "** CHAIN **"
               print tenLs
               -- 10になってる列を消去
               mapM_ (\ii -> writeArray board ii 0) $ nub $ concat tenLs
               drops <- fmap (concatMap (\((r,c), e) -> if e == 0 then [] else [(c,e)])
                             ) $ getAssocs board
               forM_ (range ((1,1), (20,10))) (\ii -> writeArray board ii 0)
               forM_ drops (\cn -> dropBoard board cn)
               chainCheck board score (ch + 1) (co + (length $ concat $ nub tenLs))
       else do when (ch /= 0) $
                 modifyIORef score ((+) $ 2 ^ (min ch 30 - 1) * (max 1 (ch - 29)) * co)
               putStrLn "SCORE"
               point <- readIORef score
               print point
               return ()
  where getLs ii iis = (virtLs ii iis):(horiLs ii iis):(diagLs ii iis):(diagLs' ii iis):[]
        virtLs ii@(r,c) iis = if elem (r+1,c) iis then ii:(virtLs (r+1,c) iis) else [ii]
        horiLs ii@(r,c) iis = if elem (r,c+1) iis then ii:(horiLs (r,c+1) iis) else [ii]
        diagLs ii@(r,c) iis = if elem (r+1,c+1) iis then ii:(diagLs (r+1,c+1) iis) else [ii]
        diagLs' ii@(r,c) iis= if elem (r+1,c-1) iis then ii:(diagLs'(r+1,c-1) iis) else [ii]
          
-- packの回転
rotatePack :: Pack -> Int -> Pack
rotatePack pack angle = (!!angle) $ iterate rotate pack
  where vecMove (r, c) = (c, -r + 5)
        rotate pack = array ((1,1), (4,4)) newAssoc
          where newAssoc = map (\ii -> ((vecMove ii), pack ! ii)) (range ((1,1), (4,4)))
        
-- packの移動
getDropPack :: Pack -> Int -> [(Int,Int)]     -- return [(col, num)]
getDropPack pack pos = map (\((r, c), num) -> (c + pos, num)) es
  where es = filter (\(ii, e) -> 0 /= e) $ assocs pack

-- 終了判定  
endCheck :: Board -> IO Bool
endCheck board = do blocks <- mapM (readArray board) (zip [16,16..] [1..10])
                    return $ or $ map (0/=) blocks
                    
                    
--- print module ---
printCommand (pos, angle) = 
  do -- putStrLn "--- COMMAND ---"
     putStrLn (show pos ++ " " ++ show angle)
     -- putStrLn ""

printBoard board =
  do putStrLn "--- BOARD ---"
     forM_ (reverse [1..16]) (\i -> do mapM_ (\ii -> do num <- readArray board ii
                                                        case num of
                                                          0          -> putStr "/"
                                                          i | i < 10 -> putStr $ show num
                                                          i | i ==10 -> putStr "+"
                                                          i          -> putStr "*"
                                             ) (zip [i,i..] [1..10])
                                       putStrLn "")
     putStrLn ""

printPack pack = 
  do putStrLn "--- PACK ---"
     forM_ (reverse [1..4]) (\i -> do mapM_ (\ii -> let num = pack ! ii 
                                                    in case num of
                                                      i | i < 10 -> putStr $ show num
                                                      i | i ==10 -> putStr "+"
                                                      i          -> putStr "*"
                                            ) (zip [i,i..] [1..4])
                                      putStrLn "")
     putStrLn ""