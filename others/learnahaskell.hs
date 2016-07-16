
-- data Path = Path Int Int Int
-- type Ans = ((String, Int), (String, Int))
  
  
-- input :: IO [Path]
-- input = do ap <- getLine
--            bp <- getLine
--            cp <- getLine
--            if cp == "0"
--              then return $ [Path (read ap) (read bp) (read cp)]
--              else do paths <- input
--                      return $ (Path (read ap) (read bp) (read cp)) : paths
                     
-- calc :: Ans -> Path -> Ans
-- calc ((aSt, aw), (bSt, bw)) (Path ap bp cp) = (nextA, nextB)
--   where nextA = if aw + ap <= bw + bp + cp
--                 then (aSt ++ "A", aw + ap)
--                 else (bSt ++ "BC", bw + bp + cp)
--         nextB = if bw + bp <= aw + ap + cp
--                 then (bSt ++ "B", bw + bp)
--                 else (aSt ++ "AC", aw + ap + cp)
  

-- main1 = do paths <- input
--            return $ foldl calc (("", 0), ("", 0)) paths
           
--all about monad ------------------------------------------           
-- test in ghci: System.Environment.withArgs ["abcd", "efgh"] main          

import System.Environment
import System.IO
import Control.Monad.Error

-- set1 内の文字と対応する set2 内の文字に変換する
translate :: String -> String -> Char -> Char
translate []     _      c = c
translate (x:xs) []     c = if x == c then ' ' else translate xs []  c
translate (x:xs) [y]    c = if x == c then  y  else translate xs [y] c
translate (x:xs) (y:ys) c = if x == c then  y  else translate xs ys  c

-- 文字列全体を変換する
translateString :: String -> String -> String -> String
translateString set1 set2 str = map (translate set1 set2) str

usage :: IOError -> IO ()
usage e = do putStrLn "Usage: ex14 set1 set2"
             putStrLn "Translates characters in set1 on stdin to the corresponding"
             putStrLn "characters from set2 and writes the translation to stdout."

-- コマンドライン引数にしたがって、標準入力を標準出力へ変換する
main :: IO ()
main = (do [set1,set2] <- getArgs
           contents    <- hGetContents stdin
           putStr $ translateString set1 set2 contents)
       `catchError` usage          