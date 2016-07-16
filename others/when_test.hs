import Control.Monad

main' :: IO ()
main' = do c <- getChar
           if c /= ' '
             then do putChar c
                     main'
             else return ()
                 
main :: IO ()
main = do c <- getChar
          when (c /= ' ') $ do putChar c
                               main