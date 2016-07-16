-- import Unsafe.Coerce

-- 論理値 --
true  = \x -> \y -> x
false = \x -> \y -> y

if' = \b -> \th -> \el -> b th el

not' = \b -> b false true
and' = \b1 b2 -> if' b1 b2 false
or'  = \b1 b2 -> if' b1 true b2

cons = \x -> \y -> \f -> f x y
car = \l -> l true
cdr = \l -> l false

toBool b = b (True) (False)
  
-- チャーチ数 --
n0 = \s -> \z -> z
n1 = \s -> \z -> s z
n2 = \s -> \z -> s.s $ z
n3 = \s -> \z -> s.s.s $ z

succ' = \n -> \s -> \z -> n s (s z)

plus = \m n -> \s -> \z -> m s (n s z)
mul  = \m n -> \s -> \z -> m (\z -> n s z) z
mul' = \m n -> m (plus n) n0
exp  = \m n -> \s -> \z -> m n s z
exp' = \m n -> n (mul  m) n1

pred'   = \n -> \s -> \z -> car (n (\c -> cons (cdr c) (s (cdr c))) (cons z z))
pred''  = \n -> car $ n (\c -> cons (cdr c) (succ' (cdr c))) (cons n0 n0)
-- sub = \m n -> unsafeCoerce n pred' m

is0 = \n -> n (k false) true
-- le  = \m \n -> 

-- mod' =
-- div' =  

-- toInt :: ((Int -> Int) -> Int -> Int) -> Int
toInt c = (c (+1) 0)

-- combinator --
i = \x -> x
k = \x y -> x
s = \x y z -> x z (y z)

b = s (k s) k
