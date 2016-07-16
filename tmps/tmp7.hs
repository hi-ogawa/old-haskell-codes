-- what i want is "f (fix f) = (fix f)" 
fix :: (a -> a) -> a
fix f = let x = f x in x


fact :: Int -> Int
fact = fix (\f -> \n -> if n <= 1 then 1 else n * (f (n - 1)))

{-
fact 3
(fix (\f -> \n -> if n <= 1 then 1 else n * (f (n - 1)))) 3
g 3
  where g = (\f -> \n -> if n <= 1 then 1 else n * (f (n - 1))) g
g 3
  where g = \n -> if n <= 1 then 1 else n * (g (n - 1))
3 * (g 2)
  where g = \n -> if n <= 1 then 1 else n * (g (n - 1))
3 * 2 * (g 1)
  where g = \n -> if n <= 1 then 1 else n * (g (n - 1))
3 * 2 * 1
-}

my_even, my_odd :: Int -> Bool
my_even 0 = True
my_even n = my_odd (n - 1)
my_odd 0 = False
my_odd n = my_even (n - 1)

my_even_fix, my_odd_fix :: (Int -> Bool) -> (Int -> Bool) -> (Int -> Bool)
my_even_fix f g 0 = True
my_even_fix f g n = g (n - 1)
my_odd_fix f g 0 = False
my_odd_fix f g n = f (n - 1)

-- fix2 :: ((Int -> Bool) -> (Int -> Bool) -> (Int -> Bool)) -> ((Int -> Bool) -> (Int -> Bool) -> (Int -> Bool))
--         -> (Int -> Bool, Int -> Bool)
fix2 :: (a -> a -> a) -> (a -> a -> a) -> (a, a)
fix2 h1 h2 = let f = h1 f g
                 g = h2 f g
             in (f, g)

(my_even', my_odd') = fix2 my_even_fix my_odd_fix



data Stream a = Cons a (Stream a)

s :: Stream Int
s = Cons 1 s

