{-
in Hask Category,

object <==> type
arrow  <==> function
arrow-composite <==> (.)

(1) f . id = id . f = f

when f is undefined, (1) is satisfied ?

id . undefined = \x -> id (undefined x)    (by definition of (.))
               = \_ -> undefined

"\_ -> undefined" is equal to "undefined" ??

when program with 'seq', these two is not equal !!

so, in order to satisfy (1), we take another 'arrow-composite'.
it is OK to take (.!).
-}

f :: Int -> Int
f = undefined

ex1 = seq (id . f) $ 1
ex2 = seq f        $ 1
