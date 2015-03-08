(*
Run-length encoding of a list. (easy)
*)

(* this solution is not tail recursive *)
let encode l = 
  let rec aux acc = function 
    | [] -> []
    | [a] -> [(acc+1, a)]
    | a::b::t -> 
      if a = b
      then aux (acc+1) (b::t)
      else (acc+1, a)::aux 0 (b::t)
  in aux 0 l
;;

(* Test *)
encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
(*- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]*)
