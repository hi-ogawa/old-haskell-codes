import Control.Monad.Trans.Free
import Control.Monad.Trans
import Control.Applicative
import Data.Functor ((<$>))

data FState a next = L (a -> (next, a))

instance Functor (FState a) where
  fmap f (L k) = L $ \x -> let (n, x') = k x in (f n, x')

type MyStateT m a = FreeT (FState a) m

myget :: Monad m => MyStateT m a a
myget = FreeT . return . Free . L $ \x -> (FreeT . return . Pure $ x, x)

myput :: Monad m => a -> MyStateT m a ()
myput s = FreeT . return . Free . L $ \_ -> (FreeT . return . Pure $ (), s)

runMyState :: Monad m => MyStateT m a ret -> a -> m (ret, a)
runMyState m s = 
  do fr <- runFreeT m
     case fr of
       (Pure x)     -> return (x, s)
       (Free (L k)) -> let (n, s') = k s in runMyState n s'

ex0 :: MyStateT IO Int ()
ex0 = do i <- myget
         lift $ print i
         i' <- read <$> lift getLine
         myput i'
         get_print
         incr
         get_print
         
get_print :: Show a => MyStateT IO a ()
get_print = do i <- myget 
               lift $ print i
  
incr :: MyStateT IO Int ()
incr = do i <- myget
          myput (i+1)