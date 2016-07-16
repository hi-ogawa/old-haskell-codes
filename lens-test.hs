import Control.Lens
import Control.Monad.State

type M a = StateT (Int, Int) IO a

runEx :: M () -> IO ()
runEx ex = evalStateT ex (0, 0)

ex0 :: M ()
ex0 = do get >>= liftIO . print
         _1 %= (+20)
         _2 .= 10
         get >>= liftIO . print
         
