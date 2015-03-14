(*

Rotate a list N places to the left. (medium)

 *)

let rev l = 
  let rec aux acc = function
    | [] -> acc
    | a::t -> aux (a::acc) t
  in aux [] l;;
;;

let split l n = 
  let rec split' count l1 l2 = function
    | [] -> (rev l1, rev l2)
    | a::t -> 
      if count < n then split' (count+1) (a::l1) l2 t
      else split' (count+1) l1 (a::l2) t
  in split' 0 [] [] l
;;

let rotate l n =
  let length_list = List.length l  in 

  let length_rotation = 
    if n < 0 then (n mod length_list) + length_list
    else n

  in

  let l1,l2 = split l length_rotation

  in

  let rec concat l1 l2 acc = match l1,l2 with
    | [],[] -> rev acc
    | a::t, [] -> concat t l2 (a::acc)
    | _, a::t -> concat l1 t (a::acc)

  in concat l1 l2 []
;;


(* Test *)

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
(*- : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]*)
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
(*- : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]*)
