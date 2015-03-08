(*
Split a list into two parts; the length of the first part is given. (easy)
*)

let rev l = 
  let rec aux acc = function
    | [] -> acc
    | a::t -> aux (a::acc) t
  in aux [] l;;

let split l n = 
  let rec split' count l1 l2 = function
    | [] -> (rev l1, rev l2)
    | a::t -> 
      if count < n then split' (count+1) (a::l1) l2 t
      else split' (count+1) l1 (a::l2) t
  in split' 0 [] [] l
;;

(* Test *)
split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
(*- : string list * string list =
(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])*)
split ["a";"b";"c";"d"] 5;;
(*- : string list * string list = (["a"; "b"; "c"; "d"], [])*)
