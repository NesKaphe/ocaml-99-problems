(*

Extract a slice from a list. (medium)

Given two indices, i and k, the slice is the list containing the elements 
between the i'th and k'th element of the original list (both limits 
included). Start counting the elements with 0 (this is the way the List 
module numbers elements).

*)

let rev l = 
  let rec aux acc = function
    | [] -> acc
    | a::t -> aux (a::acc) t
  in aux [] l;;


let slice l i k =

  let length_slice = k - i + 1

  in

  let rec pass l n_to_pass = match l with
    | [] -> []
    | a::t -> if n_to_pass = 0 then t else pass t (n_to_pass - 1)

  in

  let rec slice' l acc n_to_take = match l with
    | [] -> rev acc
    | a::t -> 
      if n_to_take = 0 then rev acc 
      else slice' t (a::acc) (n_to_take - 1) 

  in slice' (pass l (i-1)) [] length_slice
;;

(* Test *)
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
(*- : string list = ["c"; "d"; "e"; "f"; "g"]*)
