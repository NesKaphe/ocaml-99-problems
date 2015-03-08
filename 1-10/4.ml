(*
Find the number of elements of a list. (easy)
*)

let length l = 
  let rec aux acc = function
    | [] -> acc
    | a::t -> aux (acc+1) t
  in aux 0 l
;;

(* Test *)
length [ "a" ; "b" ; "c"];;
(* - : int = 3 *)
length [];;
(* - : int = 0 *)
