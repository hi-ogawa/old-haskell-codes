import Prelude hiding (break)
import Control.Monad.Trans.Free
import Control.Monad.Trans
import Control.Monad

data Zero a = Zero deriving (Show, Eq, Ord)
instance Functor Zero where
  fmap _ _ = Zero
  
type BreakT m a = FreeT Zero m a

myBreak :: Monad m => BreakT m ()
myBreak = liftF Zero

ex0 :: BreakT IO ()
ex0 = 
  do lift $ putStrLn "pleas input charcter"
     str <- lift getLine
     when (str == "ok") myBreak
     lift $ putStrLn "continuing .. "
     ex0
