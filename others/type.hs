data Btree a = Leaf | Node a (Btree a) (Btree a)  deriving Eq

empty_t :: Btree a
empty_t = Leaf

inits_t :: Ord a  => [a] -> Btree a
inits_t [] = empty_t
inits_t (x:xs) = insert_t x (inits_t xs)

insert_t :: Ord a => a -> Btree a -> Btree a
insert_t e Leaf = Node e Leaf Leaf
insert_t e1 (Node e2 t1 t2) | e1 > e2 = Node e2 t1 (insert_t e1 t2)
                            | e1 < e2 = Node e2 (insert_t e1 t1) t2
                            | otherwise = error "overlap element"

delete_t :: Ord a => a -> Btree a -> Btree a
delete_t e Leaf = error "delete_t"
delete_t e1 (Node e2 t1 t2) | e1 > e2 = Node e2 t1 (delete_t e1 t2)
                            | e1 < e2 = Node e2 (delete_t e1 t1) t2
                            | t2 /= empty_t = Node (getMin t2) t1 (deleteMin t2)
                            | otherwise = t1
  where --getMin :: Eq a => Btree a -> a
        getMin Leaf = error "delete_t: getMin"
        getMin (Node e t1 t2) | t1 == empty_t = e
                              | otherwise = getMin t1
        --deleteMin :: Eq a => Btree a -> Btree a
        deleteMin Leaf = error "delete_t: deleteMin"
        deleteMin (Node e t1 t2) | t1 == empty_t = t2
                                 | otherwise = Node e (deleteMin t1) t2
                                          
--mem_t :: ord a => a -> Btree a -> Bool
mem_t e Leaf = False
mem_t e1 (Node e2 t1 t2) | e1 > e2 = mem_t e1 t2
                         | e1 < e2 = mem_t e1 t1
                         | otherwise = True

-- depth_t :: Btree a -> Int
depth_t Leaf = 0
depth_t (Node e t1 t2) = max (depth_t t1 +1) (depth_t t1 +1)

-- length_t :: Btree a -> Int

-- t_to_l :: Btree a -> [a]

instance (Show a) => Show (Btree a) where
  show Leaf = "null"
  show (Node e t1 t2) = "(n" ++ (show (depth_t (Node e t1 t2))) ++ ", " ++ (show e) ++ " (l, " ++ (show t1) ++ "), (r, " ++ (show t2) ++ "))"