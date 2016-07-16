import Control.Monad.Reader

test = runReader r_monad2 "env"

--rmon :: Reader String Int
-- r_monad1 = do env <- ask
--               return $ env !! 2
              
r_monad2 = do env <- withReader ("local-"++) ask
              return $ env !! 2