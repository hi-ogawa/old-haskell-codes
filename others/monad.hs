-- putStr s (==) sequense_ (map putChar s)

--sequence :: (Monad m) => [m a] -> m [a]
--sequence_ :: (Monad m) => [m a] -> m ()

mySequence [] = return []
mySequence (ma:ms) = do a <- ma
                        s <- mySequence ms
                        return (a:s)
                      
mySequence1_ [] = return ()
mySequence1_ (ma:ms) = ma >> mySequence1_ ms

mySequence2_ ms = foldr (\ma r -> ma >> r) (return ()) ms

myFoldM f a [] = return a
myFoldM f a (b:bs) = do a' <- f a b
                        myFoldM f a' bs
                        
                        