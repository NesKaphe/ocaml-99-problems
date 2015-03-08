(*
Modified run-length encoding. (easy)
*)

type 'a rle =
| One of 'a
| Many of int * 'a;;

let encode l =
  let make_rle count element= 
    if count = 1 then One element
    else Many(count, element) in
  let rec aux acc = function
    | [] -> []
    | [a] -> [make_rle (acc+1) a]
    | a::b::t ->
      if a = b
      then aux (acc+1) (b::t)
      else (make_rle (acc+1) a)::aux 0 (b::t)
  in aux 0 l
;;

(* Test *)
encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
(*- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]*)
