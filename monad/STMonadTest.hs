import Data.Array
import Data.Array.ST            -- mutable
import Control.Monad.ST
import Control.Monad(forM_)
import Data.Ix(range)

main = do i <- fmap read getLine
          let arr = runSTArray $ genArray i
          -- let arr = genArray i
          return $ sum $ elems arr
          
--genArray :: Int -> ST s (STArray s (Int,Int) Int)
genArray i = do arr <- newArray ((1,1), (i,i)) 0
                forM_ (range ((1,1), (i,i))) (\(x,y) -> writeArray arr (x,y) (x+y))
                return arr

-- genArray :: Int -> Array (Int,Int) Int
-- genArray i = arr'
--   where arr = array ((1,1), (i,i)) []
--         arr'= foldl (\arr (x,y) -> arr // [((x,y), x+y)]) arr (range ((1,1), (i,i)))