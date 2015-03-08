(*
Replicate the elements of a list a given number of times. (medium)
*)

let rev l = 
  let rec aux acc = function
    | [] -> acc
    | a::t -> aux (a::acc) t
  in aux [] l;;

let replicate l n =
  let rec aux elem count acc = 
    if count < 1 then acc else aux elem (count-1) (elem::acc)
  in let rec replicate' n acc = function
  | [] -> acc
  | a::t -> replicate' n (aux a n acc) t
     in rev (replicate' n [] l)
;; 

(* Test *)
replicate ["a";"b";"c"] 3;;
(*- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]*)
