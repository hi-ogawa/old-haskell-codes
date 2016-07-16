main = getLine >>= (\s -> if read s == 42 
                          then return ()
                          else do putStrLn s
                                  main
                   )