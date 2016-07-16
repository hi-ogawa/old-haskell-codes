allCombiF :: (a -> a -> a) -> [[a]] -> [a]
allCombiF f []          = []
-- allCombiF f (ls:[])     = ls
-- allCombiF f (ls:lss)    = do x <- ls
--                              y <- allCombiF f lss
--                              return $ f x y
allCombiF f (ls:lss)    = foldl subf ls lss                             
  where subf l1 l2 = do x1 <- l1                          
                        x2 <- l2
                        return $ x1 `f` x2
                             
-- instance Monad [] where                            
--   return :: a -> [a]
--   >>= :: [a] -> (a -> [b]) -> [b]
  
--   return x = [x]
--   ls >>= f = concatMap f ls


-- :m +Control.Monad.State Control.Monad.Identity