data EqC a = EqC ((a -> a -> Bool), (a -> a -> Bool))

over eq :: EqC a in
inst eq :: EqC a => EqC [a]   -- forall a.(eq :: EqC a).EqC [a]
                              -- (eq :: EqC a).EqC [a]
     = 

