-- ref: http://itpro.nikkeibp.co.jp/article/COLUMN/20070909/281498/?ST=develop&P=2

-- 「かつ」
type And a b = (a, b)

-- 「または」
data Or a b = L a | R b

-- 実際に存在する型 = 真
-- ()

-- 定義不可能な型 = 偽(論理式における)
data Void = V Void


hoge :: (q -> (p -> Void) -> Void) -> ((q -> p) -> Void) -> Void
hoge f g = 


-- (~P -> ~Q) <-> (Q -> ~~P) <-> ~~(Q -> P)






-- 否定
data Not a = N (a -> Void)

-- ¬F => T
-- test :: Not () -> Void
-- test = \(N x) -> x ()

-- 二重否定の除去(を公理とする) -> 古典論理のような証明へ
-- これを仮定しないものは直観主義論理
notnot :: Not (Not a) -> a
notnot = undefined

-- 排中律(¬A ∨ A): これに二重否定をかける
x :: Not (Not (Or a (Not a)))
x = N (\(N y) -> y (R (N (\z -> y (L z)))))
-- すると、 notnot x :: Or a (Not a)
-- としてプログラム(証明)を定義することができた。型(命題)が証明された。
-- まあ、きもい。 型理論つっこみたいなぁ。


-- test :: (a, (a -> b)) -> b
-- test = \x -> (snd x) (fst x)
-- 証明図と対応づけて考えてみたほうが良さそう
{-
a ^ (a => b)
------------(fst, snd)
 a      a => b
---------------(apply)
    b


-- 以下[http://www.kmonos.net/wlog/61.html#_0538060508]より
(自然演繹の規則:        それに対応するプログラムの演算)
かつ除去:       fst, snd
かつ導入:       (,)
ならば除去:     apply
ならば導入:     lambda abstraction      ??「"ならば"導入則 : aを仮定してbが証明できたらそれは aならばb の証明」は、λ抽象／関数定義です
または除去:     以下のorJyokyo参照
または導入:     L, R    (Or型のラベル)

<または除去について>
orJyokyo :: ((Or a b), ((a -> c), (b -> c))) -> c
orJyokyo = \x -> 
  case fst x of
    L y -> (fst . snd $ x) y
    R z -> (snd . snd $ x) z

-- 論理演算子について
and: tuple
or:  variant
not: ??継続  とか  a -> _|_  とか

test :: (a -> b) -> (Not b -> Not a)  -- 対偶的ななにか
test = \f -> \(N g) -> \x -> g . f $ x

-- 二重否定除去則, 背理法 <=> call/cc

**call/cc の例 (scheme)
(define savecc ())

(define (fact n)
  (if (<= n 1)
      (call/cc 
       (lambda(cc)
	 (set! savecc cc)
	 1))
      (* n (fact (- n 1)))))

**call/ccに型について
call/ccはラムダ抽象を引数に取る
そのラムダ抽象をfとすると、そのfは継続ccを引数にとり、fの返り値の型は継続ccの引数となるものと同じになっているはず。
f :: (a -> b) -> a
そして、call/cc自体も値として継続ccの引数と同じ型のものを返すはずである。
よって、こんな感じ。
-}
-- callcc :: ((a -> _|_) -> a) -> a
callcc :: (Not a -> a) -> a
callcc = undefined

{-
**二重否定除去を用いなければ証明できない命題の例
二重否定除去[~~a => a]
これは任意の命題がtrueまたはfalseに入るってことを言ってるような気がする。
よって、[~a | a]を証明するのには二重否定除去が必要なんじゃないかなぁ。
ってことで、これが良い例になるようです。
?? :: Or (a -> _|_) a
-}
-- tmp :: ((Or (a -> _|_) a) -> _|_) -> (Or (a -> _|_) a)
tmp :: (Not (Or (Not a) a)) -> (Or (Not a) a)
tmp = \(N f) -> L (N (\x -> f (R x)))

{-
Not aの実体は「継続」であることを考えるとプログラム組みやすく感じる。ってかそりゃそうか、証明図が実体なんだから。
あとはtmpをcallccにかければおｋ。こうしてみるとcallccによって背理法そのものを公理とする感じか。

あと、
[単純型付きλ計算+callcc]をつかって[T]が証明できるならば、       (古典論理)
[単純型付きλ計算のみ]をつかって[~~T]が証明できる。              (直観主義論理)
らしい。

んー、でも、あのコピペ(悪魔の話)の旨みが把握できないんだよな。


-- その他にもプログラムと論理のアナロジーはいっぱいあるみたい
「帰納法 ⇔ fold」
「二階命題論理 ⇔ 多相型」
「一階述語論理 ⇔ Dependent Type」
「線形論理 ⇔ 線形型」 
などなど。

-}
