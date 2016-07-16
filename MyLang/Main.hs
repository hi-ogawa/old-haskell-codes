module Main where

import qualified Types as TY
import qualified ParsecParser as PP
import qualified Typing as TYC
import qualified EvalLazy as EL

parseLine :: IO ()
parseLine = print . TY.runLangM . PP.doParseMain =<< getLine

parseFile :: FilePath -> IO ()
parseFile file = print . TY.runLangM . PP.doParseMain =<< readFile file

execute :: String -> TY.LangM TY.Result
execute inp = do (tinfo, exp) <- PP.doParseMain inp
                 TY.putInfo tinfo
                 EL.evalMain =<< TYC.typeCheckMain exp

printResult :: TY.LangM TY.Result -> IO ()
printResult = either print putStrLn . TY.runLangM

evalFile :: FilePath -> IO ()
evalFile file = printResult . execute =<< readFile file

evalLine :: IO ()
evalLine = printResult . execute =<< getLine
