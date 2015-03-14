(*

Create a list containing all integers within a given range. (easy)

If first argument is smaller than second, produce a list in decreasing order.

*)


let range n m =
  let rec aux acc n m =
    if n > m then acc else aux (n::acc) (n+1) m
  in if n < m then List.rev (aux [] n m) else aux [] m n
;;


(* Test *)
range 4 9;;
(*- : int list = [4; 5; 6; 7; 8; 9]*)
range 9 4;;
(*- : int list = [9; 8; 7; 6; 5; 4]*)
