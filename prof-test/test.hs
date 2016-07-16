main = print (fib 25)

fib n | n <= 1 = 1
      | otherwise = fib (n-1) + fib (n-2)