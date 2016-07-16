fix1 :: (a -> a) -> a
fix1 f = f (fix1 f)

fix2 :: (a -> a) -> a
fix2 f = let x = f x in x

{-
fix (\x -> cycle (1:x))
=> (\x -> cycle (1:x)) (fix f')
=> cycle (1:(fix f'))
=> (1:(fix f')) ++ cycle (1:(fix f'))
..
-}            

-- factorial --
myfact' :: (Int -> Int) -> Int -> Int
myfact' f = \n -> if n <= 1 then 1 else n * f (n-1)
myfact = fix1 myfact'

-- map -- 
mymap' :: ((a1 -> a) -> [a1] -> [a]) -> (a1 -> a) -> [a1] -> [a]
mymap' rec = \f l -> if null l then [] else f (head l) : rec f (tail l)
mymap = fix1 mymap'     -- (mymap f = fix1 mymap' f)

-- fib --
myfib' rec = \n -> if n <= 1 then 1 else rec (n-1) + rec (n-2)
myfib = fix1 myfib'

-- filter --
myfilter' rec = \p l -> if null l then [] else 
                          if p (head l) then (head l) : rec p (tail l) else rec p (tail l)

myfilter = fix1 myfilter'       -- (myfilter f = fix1 myfilter f)

-- foldr --
myfoldr' rec = \f b l -> if null l then b else (head l) `f` rec f b (tail l)
myfoldr = fix1 myfoldr'

mfoldr f b []     = b                         
mfoldr f b (a:as) = a `f` mfoldr f b as
  
{-
fix fact' = fact' (fix fact')
          = (\f n -> if n <= 1 then 1 else n * f (n-1)) (fix fact')
          = \n -> if n <= 1 then 1 else n * (fix fact') (n-1)
          = \n -> if n <= 1 then 1 else n * ((\f n -> if n <= 1 then 1 else n * f (n-1)) (fix fact')) (n-1)
          = \n -> if n <= 1 then 1 
                  else n * (\n' -> if n' <= 1 then 1 else n' * (fix fact') (n'-1)) (n-1)
          = ..
おっ、なんか展開できそうじゃん            
-}  

{-
fix fib' = fib' (fix fib')
         = (\n -> if n <= 1 then 1 else (fix fib') (n-1) + (fix fib') (n-2))
         = (\n -> if n <= 1 then 1 else (fib' (fix fib')) (n-1) + (fib' (fix fib')) (n-2))
         = (\n -> if n <= 1 then 1 else 
                    (\na -> if na <= 1 then 1 else (fix fib') (na-1) + (fix fib') (na-2)) (n-1)
                    +
                    (\nb -> if nb <= 1 then 1 else (fix fib') (nb-1) + (fix fib') (nb-2)) (n-2)
           )
         = ..               
fibになりそう
-}

-- fix型の話 (from haskell wiki) --
newtype Fix f = Fix (f (Fix f))
-- いや、わけわからん

-- fix: it finds a least-defined fixed point of a function
-- Types in Haskell have a partial order on them called definedness. In any type, ⊥ is the least-defined value (hence the name "bottom").
-- So since ⊥ is the least-defined value for all types and fix finds the least-defined fixed point, if f ⊥ = ⊥, we will have fix f = ⊥ (and the converse is also true). If you've read the denotational semantics article, you will recognise this as the criterion for a strict function: fix f diverges if and only if f is strict.
-- fix に与えるのが正格な関数化どうかが重要みたい(正格だったら全部発散) (fix (*3) => 発散(ボトム))

{- in ocaml
-- <0>: 普通の再帰定義 --
let rec fix f =
 fun x -> (f (fix f)) x;;       -- closure を返すのが一般的

-- <1>: 参照を利用 --
let fix f =
  let t = ref (fun x -> raise(Failure ""))
  in
  t :=  (fun x -> f !t x);
  !t;;

-- <2>: 再帰型の利用 --
type 'a recc = In of ('a recc -> 'a);;
let fix f =
  let out (In x) = x
  in
  (fun x a -> f (out x x) a) (In (fun x a -> f (out x x) a));;
-}

{- in scheme
-- <0>: 普通のやつ --
(define (fix f)
  (lambda (x) ((f (fix f)) x)))

-- <1>: 再帰を使わない定義 --
??
-}

-- http://d.hatena.ne.jp/haxis_fx/20110726/1311657175 --

-- (\x -> x*x) (fix1 f)
-- => (fix1 f) * (fix1 f)
-- => ..



-- 再帰型の利用 (from wiki) --

-- In :: (Rec a -> a) -> Rec a
-- out :: Rec a -> (Rec a -> a)

newtype Rec a = In {out :: Rec a -> a}

y :: (a -> a) -> a
y = \(f::a -> a) -> (\(x::Rec a) -> f (out x x)) (In (\(x::Rec a) -> f (out x x)))

-- y g = 

-- y g => (\x -> g (x x)) (\x -> g (x x))
--     => g ((\x -> g(x x)) (\x -> g(x x)))
--     => g (y g)