(*

Generate a random permutation of the elements of a list. (easy)

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

let permutation l = rand_select l (List.length l);;

(* Test *)
permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
(*- : string list = ["a"; "e"; "f"; "b"; "d"; "c"]*)
