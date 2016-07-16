data MyData a = J a | N 
              deriving Show

{-
class Functor (f :: * -> *) where
fmap :: (a -> b) -> (f a -> f b)
-}
instance Functor MyData where
  fmap f (J x) = J (f x)
  fmap f N     = N

{-
class Functor m => Monad (m :: * -> *) where
return :: a -> f a
(>>=)  :: m a -> (a -> m b) -> m b

?? 
join :: m (m a) -> m a


[Category] C
[Functor] M: C -> C
Cの任意の対象Xに対して２つの射が存在  -- (0)
unit_x: X -> M(X)
join_x: M(M(X)) -> M(X)

モナド則
(1) join_x * M(join_x) = join_x * join_mx
(2) join_x * M(unit_x) = join_x * unit_mx  = id_x
∀f: a -> b に対し
(3) unit_b * f = M(f) * unit_a
(4) join_b * M(M(f)) = M(f) * join_a

haskellではjoin, unit(return)は多相化されているので添字はいらない?

(1) join . fmap join = join . join
(2) join . fmap return = join . return = id
∀f: a -> b に対し
(3) return . f = fmap f . return
(4) join . fmap (fmap f) = fmap f . join
-}

instance Monad MyData where
  return x = J x
  mx >>= f = join (fmap f mx)
  
join :: MyData (MyData a) -> MyData a
join (J (J x)) = J x
join (J N)     = N
join N         = N

{-
(J x) >>= f = f x
N     >>= f = N

join mmx = mmx >>= id

上で定義するjoinが同じ関数であることを確認したい。(とりあえずmaybeについてだけ)
∀xに対して
join (J (J x))  = id (J x) = J x
join (J N)      = id N     = N
join N          = N        = N



で、Maybe-functorにおいては、任意の対象(Haskでは型)について
[unit,join]なる射が存在し、いい感じの性質(モナド則)を持つことが分かった。

次はIOについて考えてみる。(~/Desktop/imperative-functional-programming.psより)
in functional language, an expression denotes a value.
but an IO-Command should perform an action.

primitive IO action is only [getcIO] and [putcIO].
in this system, the value of the entire program is a single action.(is called [main])
when program is executed, [main] IO-action is performed.
 - "perform"と"action"の概念は区別すべし
 - IO-action自体は第一級のオブジェクトとして考えられる
 - IO-actionがperformされたときに値が返されることもあるのでそれを扱いたい(bindIOへの動機)

for combining IO-action, we provide two function [unitIO] and [bindIO].
unitIO :: a -> IO a
 - (x :: a)に対して、(unitIO x)はperformされたときにxを返すようなIO-actionを表す。

bindIO :: IO a -> (a -> IO b) -> IO b
 - (ma :: IO a), (famb :: a -> IO b)に対して
   (ma `bindIO` famb)はperformされたときに以下を実現するIO-actionを表す。
   - まず、maをperformする。
   - 上のperformで得られたvalueをxとし、(famb x)なるio-actionをperformする。

これがmonadになることの確認。

   joinIO mmx = mmx `boinIO` id
mx `bindIO` f = joinIO (fmap f mx)


まず、
fmap :: (a -> b) -> IO a -> IO b
の定義をしなきゃいけないよ。

(1 ) join . fmap join = join . join
(1') 

-}


{-
instance Functor ((->) r) where
 fmap f x = f . x

f :: a -> b
x :: r -> a
fmap f x :: (r -> b)

instance Monad ((->) r) where
 return a = const a
 k >>= f  = \r -> (f (k r)) r

k :: r -> a
f :: (a -> (r -> b)) -> (r -> b)

instance Applicative ((->) r) where
 f <*> g == liftM2 id f g
         == do x <- f; y <- g; return (x y)
         == f >>= (\x -> g >>= (\y -> return (x y)))
         == \r -> ((g >>= (\y -> return ((f r) y))) r)
         == \r -> ((\s -> (return ((f r) (g s))) s) r)
         == \r -> const ((f r) (g r)) r
         == \r -> (f r) (g r)

f :: r -> (a -> b)
g :: r -> a
f <*> g :: r -> b


-}