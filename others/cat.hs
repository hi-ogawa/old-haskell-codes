import System.Environment

catFile filename = do content <- readFile filename
                      putStrLn content

catNFile :: String -> IO ()
catNFile filename = do content <- readFile filename
                       mapM_ putStrLn $ zipWith (\n line -> show n ++ "\t" ++ line) [0..] (lines content)

main = do args <- getArgs
          if "-n" == head args
            then mapM_ catNFile (tail args)
            else mapM_ catFile args
         