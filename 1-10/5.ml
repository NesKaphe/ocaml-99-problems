(*
Reverse a list. (easy)
OCaml standard library has List.rev but we ask that you reimplement it.
*)

let rev l = 
  let rec aux acc = function
    | [] -> acc
    | a::t -> aux (a::acc) t
  in aux [] l
;;

(* Test *)
rev ["a" ; "b" ; "c"];;
(*- : string list = ["c"; "b"; "a"]*)
