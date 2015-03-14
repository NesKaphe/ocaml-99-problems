(*

Lotto: Draw N different random numbers from the set 1..M. (easy)

The selected numbers shall be returned in a list.

*)


let range n m =
  let rec aux acc n m =
    if n > m then acc else aux (n::acc) (n+1) m
  in if n < m then List.rev (aux [] n m) else aux [] m n
;;

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

let lotto_select n m = rand_select (range 1 m) n;;

(* Test *)
lotto_select 6 49;;
(*- : int list = [10; 20; 44; 22; 41; 2]*)
