module Main where

import MySyntax
import Data.Char
import System.IO
import System.Random

colors = [Red,Blue,Yellow]
mkRandomBoard :: Int -> Int -> IO Board
mkRandomBoard hei wid =
  do seed <- getStdGen
     return $ take hei $ divList wid $ map (colors!!) $ randomRs (0, length colors - 1) seed
       where divList :: Int -> [a] -> [[a]]
             divList _ [] = []
             divList n ls = (take n ls) : (divList n $ drop n ls)
           
main :: IO ()
main =do putStr "please enter the HEIGHT and WIDTH\n"
         hei <- getLine
         wid <- getLine
         board <- mkRandomBoard (read hei) (read wid)
         score <- samegame board 0 
         putStr $ "Your Score: " ++ (show score) ++ "\n"

samegame :: [[Ball]] -> Int -> IO Int
samegame board score = do printBoard board
                          putStr $ "now: " ++ show score ++ "\n"
                          if endJudge board 
                            then do putStrLn "You can't take a Ball ANY MORE, THE END!"
                                    return score
                            else do putStr "please command > "
                                    hFlush stdout
                                    xy <- getLine    -- 1b, 3c etc ..
                                    if length xy == 2
                                      then do let x = (ord $ (!!) xy 0) - ord '0'
                                              let y = (ord $ (!!) xy 1) - ord 'a'
                                              if pickable board (x,y)
                                                then do let score' = calcScore $ markBoard board (x,y)
                                                        samegame (pickBall board (x,y)) (score + score')
                                                else do putStrLn "Can't take the Ball!!"
                                                        samegame board score
                                      else do putStr "Invalid command! retry\n"
                                              samegame board score
                                           
                                        
-- ----<board>----
--    a b c d ...
-- 0 [o,o,o,x,x]
-- 1 [x,o,o,+,x]
-- 2 [x,+,+,x,x]
-- 3 [+,+,o,o,+]
-- 4 [o,x,x,o,+]
