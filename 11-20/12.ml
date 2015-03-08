(*
Decode a run-length encoded list. (medium)
*)

type 'a rle =
| One of 'a
| Many of int * 'a;;

let rev l = 
  let rec aux acc = function
    | [] -> acc
    | a::t -> aux (a::acc) t
  in aux [] l
;;

let decode l = 
  let rec aux acc elem count =
    if count = 1 then elem::acc else aux (elem::acc) elem (count-1)
  in let rec decode' acc = function
  | [] -> acc
  | One a :: t -> decode' (a::acc) t
  | Many(count, a) :: t -> decode' (aux acc a count) t
     in rev (decode' [] l)
;;

(* Test *)
decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
(*- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]*)
