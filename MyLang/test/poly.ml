(*
let length ls =
  match ls with
  | []    -> 0
  | x:xs  -> 1 + length xs
in
(length [1,2,3], length [True,False])
*)


let bar i = 
  let foo j k = []
  in
  foo i i
in
let nil1 = bar 1
in
let nil2 = bar True
in
(1:nil1, True:nil1, 1:nil2, True:nil2)



