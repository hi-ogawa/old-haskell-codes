import Data.Maybe (fromJust)

maybeFix :: (a -> Maybe a) -> Maybe a
maybeFix f = ma
  where ma = f (fromJust ma)

listFix :: (a -> [a]) -> [a]
listFix f = ma
  where ma = f (head ma)
