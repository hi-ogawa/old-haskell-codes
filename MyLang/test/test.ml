let fibs = 1:1:zipWith (\x -> \y -> x + y) fibs (tail fibs)
and take n ls =
  if n < 1
  then []
  else 
    match ls with
      | []   -> []
      | h:tl -> h : take (n - 1) tl
and tail ls =
  match ls with
    | []   -> error 
    | h:tl -> tl
and zipWith f l1 l2 =
  match l1 with
    | [] -> []
    | h1:tl1 -> 
      match l2 with
	| []     -> []
	| h2:tl2 -> f h1 h2 : zipWith f tl1 tl2
in
take 10 fibs
