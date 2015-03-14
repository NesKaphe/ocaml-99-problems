(*

Extract a given number of randomly selected elements from a list. (medium)

The selected items shall be returned in a list. We use the Random module 
but do not initialize it with Random.self_init for reproducibility.

*)

let rand_select l n = 
  let rec take n acc = function
    | [] -> failwith "Not enough elements in the list"
    | a::t -> if n = 0 then (a, (List.rev acc) @ t) else take (n-1) (a::acc) t

  in

  let rec do_take n acc l len =
    if n > 0 then 
      let elem, l' = take (Random.int len) [] l
      in do_take (n-1) (elem::acc) l' (len - 1)
    else
      acc

  in do_take n [] l (List.length l)
;;

(* Test *)
rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
(*- : string list = ["g"; "d"; "a"]*)
