
-- primitive stack function

data Stack a = Stack [a] deriving Show

empty :: Stack a
empty = Stack []

empty' :: StackOp a ()
empty' = \st -> ((), empty)
  
pop :: StackOp a a
pop (Stack (x:xs)) = (x, Stack xs)

push :: Stack a -> a -> Stack a
push (Stack s) e = Stack (e:s)

push' :: a -> StackOp a ()
push' x = \st -> ((), push st x)


topis :: (a -> Bool) -> Stack a -> (Bool, Stack a)
topis p st0 = let (x,st1) = pop st0
              in (p x, st1)

stack = Stack [5,4,3,2,1]

-- for abstraction

type StackOp a b = Stack a -> (b, Stack a)

comb :: StackOp a b
        -> (b -> StackOp a c)
        -> StackOp a c
comb f1 f2 = \st0 -> let (x1,st1) = f1 st0
                         (x2,st2) = f2 x1 st1
                     in (x2, st2)
                        
comb_ :: StackOp a b
         -> StackOp a c
         -> StackOp a c
comb_ f1 f2 = \st0 -> let (_,st1) = f1 st0         
                          (x2,st2) = f2 st1
                      in (x2,st2)
                         
ret :: b -> StackOp a b
ret x = \st -> (x, st)


test1 = pop `comb` (\x1 ->  
        pop `comb` (\x2 -> 
        pop `comb` (\x3 ->
        ret (x1*x3+x2))))

test2 = topis (==5) `comb` (\b1 ->
        topis (==4) `comb` (\b2 ->                     
        topis (==3) `comb` (\b3 ->                             
        ret $ and [b1,b2,b3])))                     
        
test3 = empty' `comb_` push' 11 `comb_` push' 10 `comb_` push' 9 `comb_` pop `comb` (\x -> topis (/=10) `comb` (\b -> ret $ if b then x else -x))


