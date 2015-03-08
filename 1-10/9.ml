(*
Pack consecutive duplicates of list elements into sublists. (medium)
*)

let rev l = 
  let rec aux acc = function
    | [] -> acc
    | a::t -> aux (a::acc) t
  in aux [] l
;;

let pack l = 
  let rec aux sublist acc l = match l with
    | [] -> []
    | [a] -> (a::sublist)::acc
    | a::(b::t) -> 
      if a = b 
      then aux (a::sublist) acc (b::t)
      else aux [] ((a::sublist)::acc) (b::t)
  in rev (aux [] [] l)
;;


(* Test *)
pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
(*- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]*)
