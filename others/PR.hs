type N = Int
type PRtype = ([N] -> N, Int)
  
zero :: PRtype
zero = ((\_ -> 0), 0)

suc :: PRtype
suc = ((\(n:[]) -> n+1), 1)

proj :: Int -> Int -> PRtype
proj n i = ((\ns -> ns !! (i-1)), n)

com :: PRtype -> [PRtype] -> PRtype
com (g, m) gns = ((\ns -> g $ map (\(gi, n) -> gi ns) gns), length gns)


recur :: PRtype -> PRtype -> PRtype
recur (g, gn) (h, hn) = (f, gn+1)
  where f :: [N] -> N
        f ns | y == 0           = g xs
             | otherwise        = h $ xs ++ [y-1] ++ [f $ xs ++ [y-1]]
               where y = last ns
                     xs = init ns
                     
apply :: PRtype -> [N] -> N                     
apply (f, n) ns = f ns

add :: PRtype
add = recur (proj 1 1) (com suc [proj 3 3])
