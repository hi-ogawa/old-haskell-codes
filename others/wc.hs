import Data.Char
import System.Environment


bytesCount :: String -> Int
bytesCount [] = 0
bytesCount (c:cs) = 1 + bytesCount cs

wordsCount :: [Char] -> Int
wordsCount cs = outWords cs
  where outWords [] = 0
        outWords (c:cs) | isAlphaNum c = 1 + inWords cs
                        | otherwise = outWords cs
        inWords [] = 0
        inWords (c:cs) | isAlphaNum c = inWords cs
                       | otherwise = outWords cs

linesCount :: [Char] -> Int
linesCount cs = outLine cs
  where outLine [] = 0
        outLine (c:cs) | c == '\n' = 1 + inLine cs
                       | otherwise = outLine cs
        inLine [] = 0
        inLine (c:cs) | c == '\n' = inLine cs
                      | otherwise = outLine cs

wcFile :: String -> IO ()
wcFile filename =
  do cs <- readFile filename
     putStrLn ("\t" ++ show (linesCount cs) ++ 
               "\t" ++ show (wordsCount cs) ++
               "\t" ++ show (bytesCount cs) ++
               "\t" ++ filename)

main = do fs <- getArgs
          mapM_ wcFile fs
