main = print $ known_undersaturated_app () 0

{-# NOINLINE known_fun_2 #-}
known_fun_2 :: a -> a -> a
known_fun_2 x _ = x

{-# NOINLINE known_undersaturated_app #-}
known_undersaturated_app :: () -> Int -> Int
known_undersaturated_app _ = known_fun_2 10
