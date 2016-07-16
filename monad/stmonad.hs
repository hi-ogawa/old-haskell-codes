import Control.Monad.ST
import Data.STRef



-- r :: STRef s Int
-- r = runST (newSTRef 1)
{-
runST :: (forall s. ST s a) -> a
newSTRef :: a -> ST s (STRef s a)

(newSTRef 1) :: ST s (STRef s Int)

runST (newSTRef 1) :: STRef s Int
 -- forallのsが外に出てしまう
-}


-- v = runST $ newSTRef 1 >>= \r -> readSTRef r

x = do r <- newSTRef 1
       return r

overwrite :: Num a => STRef s a -> ST s ()
overwrite r = do pre <- readSTRef r
                 writeSTRef r (pre + 1)

sub :: ST s Integer
sub = do r <- x
         overwrite r
         readSTRef r

mainn = runST sub  
