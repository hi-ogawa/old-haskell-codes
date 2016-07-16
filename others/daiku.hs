import Data.Array
import Data.Maybe

type Node = Int
type Cost = Int
type Edge = (Node, Node, Cost)
type OneRow = [(Node, (Maybe Cost, Bool))]

nodes = [1,2,3,4,5]
edges = [(1,2, 5), (1,3, 10), (2,3, 3), (3,4, 2), (4,5, 4), (3,5, 7)]

findEdgeCost :: [Edge] -> Node -> Node -> Maybe Cost
findEdgeCost [] n1 n2     = Nothing
findEdgeCost ((n1,n2, cost):es) m1 m2 | n1==m1 && n2==m2 || n1==m2 && n2==m1 = Just cost
                                      | otherwise            = findEdgeCost es m1 m2
                                                               
endCheck :: OneRow -> Bool
endCheck row = and $ map (\(_, (_, b)) -> b) row

getMed :: OneRow -> (Node, (Maybe Cost, Bool))
getMed row = head $ filter (\(n, (mc, _)) -> case mc of Just c -> c == min 
                                                        Nothing -> False) row
  where min = minimum $ catMaybes $ map (\(_, (mc, b)) -> if b then Nothing else mc) row
    
getNextRow :: OneRow -> [Edge] -> Node -> OneRow
getNextRow row edges start = map subf row
  where (med, (medc, _)) = getMed row
        subf (n, (mc, b)) | n == med  = (n, (mc, True))
                          | b == True = (n, (mc, b))
                          | otherwise = (n, (nextc, False))
          where nextc = maybeMin (medc `maybePlus` findEdgeCost edges med n) mc

getCost :: OneRow -> Node -> Cost
getCost row end = case lookup end row of
  Just (Just cost, _)    -> cost
  otherwise              -> error "not occurred"

loop :: OneRow -> [Edge] -> Node -> Node -> Cost
loop row edges start end = if endCheck row
                           then getCost row end 
                           else loop nextRow edges start end
 where nextRow = getNextRow row edges start

main' = loop initRow edges start end
initRow = map (\n -> if n == start then (n, (Just 0, False)) else (n, (Nothing, False))) nodes
start = 1
end = 5

maybePlus :: Num a => Maybe a -> Maybe a -> Maybe a
maybePlus (Just a1) (Just a2) = Just $ a1 + a2
maybePlus _         _         = Nothing
maybeMin :: Ord a => Maybe a -> Maybe a -> Maybe a
maybeMin (Just a1) (Just a2) = Just $ min a1 a2
maybeMin (Just a1) _         = Just a1
maybeMin _         (Just a2) = Just a2
maybeMin _         _         = Nothing
