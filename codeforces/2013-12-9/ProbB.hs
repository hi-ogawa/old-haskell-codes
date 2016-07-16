main = do (rn:bn) <- (map read . words)`fmap` getContents
          (petya, vasya) <- solve rn bn
          putStrLn (show petya ++ " " ++ show vasya)
          
solve :: Int -> Int -> (Int, Int)
solve rn bn = undefined
  where [s, b] = sort [rn, bn]
  

{- sample 
red blue
2   4

                    petya vasya
r b r b b b     =>  2     3

r b b r b b


red blue
3 1


pair<int, int> solve(int n, int m) {    -- red = 1, blue = 3
  int r[2] = {n - 1, m};                -- r[0, 1] = {0, 3}
  int prv = 0;                          -- first cube = red ?? 
                                        --
  int p = 0, v = 0;                     --
  for (int i = 1; r[0] + r[1]; ++i) {   -- 
    int nxt = 0;                        --
    if (i % 2 == 0) {                   -- <even>
      // Petya                          --
      if (r[prv]) nxt = prv;            --
      else nxt = 1 - prv;               --
    } else {                            -- <odd>
      // Vasya                          --
      if (r[1 - prv]) nxt = 1 - prv;    -- 
      else nxt = prv;                   --
    }                                   --
    if (prv == nxt) ++p;                --
    else ++v;                           --
                                        --
    --r[nxt];                           --
    prv = nxt;                          --
  }                                     --
                                        --
  return mp(p, -v);                     --
}
-}