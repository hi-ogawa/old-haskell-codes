import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

-- main = do (_:as) <- (map (fst . fromJust . BS.readInt) . BS.words) `fmap` BS.getContents
--           putStrLn $ solve'' (reverse as)

main = do (_:as) <- (map read . words) `fmap` getContents
          putStrLn $ solve'' (reverse as)

solve' as     = fst $ foldl (\(ops, s) a ->
                              if a <= s
                              then ('-':ops, s-a)
                              else ('+':(map rev ops), a-s)
                            ) ("+", head as) (tail as)
rev '+' = '-'
rev '-' = '+'

solve'' as = (\(ops, s) -> 
               if s <= 0
               then map rev ops
               else ops
             ) 
             $ foldl (\(ops, s) a ->
                       if s <= 0
                       then ('+':ops, s+a)
                       else ('-':ops, s-a)
                     ) ("+", head as) (tail as)