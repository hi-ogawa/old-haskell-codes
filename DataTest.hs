data Hoge = Hoge {a :: Int, b :: Bool, c :: String}
          deriving Show

hoge = Hoge 1 True "piyo"

ex0 = do print hoge
         let hoge' = hoge {a = 2}
         print hoge'
         
data Tree a = Node a (Tree a) (Tree Int)
            | Leaf
              deriving Show
                       
data TreeF a = NodeF a (TreeF (a->a)) (TreeF (a->a))
             | LeafF
               
tr = NodeF 1 LeafF (NodeF succ LeafF LeafF)

ex1 = 
  case tr of
    NodeF i _ (NodeF f _ _) -> f i
               
               