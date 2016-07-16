-- from fl-2012-9.pdf report
import Control.Monad
import Control.Applicative
import Data.List(splitAt)

-- nats :: [Int]
-- nats = 0:[i+1| i <- nats]
-- nats = 0:(do i <- nats; return (i+1))
-- nats = 0:(pure (+1) <*> nats)
-- nats = 0:(concatMap (\i -> [i+1]) nats) -- monad desuger
-- nats = 0:(map (+1) nats)

-- monadplusならばmonad
-- monadならばapplicative (ここは自明だが型クラスの依存関係としてあるわけではない)
nats :: (Applicative m, MonadPlus m) => m Int
nats = return 0 `mplus` (pure (+1) <*> nats)
  
permute :: MonadPlus m => [a] -> m [a]
permute []    = mzero
permute (h:[])= return . return $ h
permute (h:l) = ins h =<< permute l
  where ins h l = foldr mplus mzero $ map (return . (\(fs, sn) -> fs ++ [h] ++ sn) . flip splitAt l) [0..(length l)]

nat_lists :: (Applicative m, MonadPlus m) => m [Int]
nat_lists = return [] `mplus` (pure (:) <*> nats <*> nat_lists)
