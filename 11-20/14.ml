(*
Duplicate the elements of a list. (easy)
*)

let rec duplicate = function
  | [] -> []
  | a::t -> a::a::duplicate t
;;


(* Test *)
duplicate ["a";"b";"c";"c";"d"];;
(*- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]*)
