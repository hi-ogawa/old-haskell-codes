data Tree {a}  = Node a Tree {a} Tree {a}
               | Leaf

data TreeF {a} = NodeF a TreeF {a->a} TreeF {a->a}
	       | LeafF

let tr1 = Node 1 (Node 2 Leaf (Node 3 Leaf Leaf)) Leaf
and tr2 = Node True (Node False Leaf (Node True Leaf Leaf)) Leaf
and tr3 = Node [1,2] (Node [3,4] Leaf (Node [5,6] Leaf Leaf)) Leaf
and tr4 = NodeF 1 LeafF (NodeF (\x -> x + 1) LeafF LeafF)
and takeleftI tr =
  match tr with
    | Leaf             -> Leaf
    | Node {_, tr1, _} -> tr1
and takeleftB tr =
  match tr with
    | Leaf            -> Leaf
    | Node {_, tr1, _} -> tr1
and sumtr tr =
  match tr with
    | Leaf                 -> 0
    | Node {n:m:_, t1, t2} -> n + m + sumtr t1 + sumtr t2
and testf tr =
  match tr with
    | NodeF {i, _, NodeF{f, _, _}} -> f i
in
(takeleftI tr1, takeleftB tr2, sumtr tr3, testf tr4)
