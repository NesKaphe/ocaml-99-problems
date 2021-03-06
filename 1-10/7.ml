(*
Flatten a nested list structure. (medium)
*)

(* 
 There is no nested list type in OCaml, so we need to define one
 first. A node of a nested list is either an element, or a list of
 nodes.
 *)

type 'a node =
| One of 'a 
| Many of 'a node list
;;

let rev l = 
  let rec aux acc = function
    | [] -> acc
    | a::t -> aux (a::acc) t
  in aux [] l
;;

let flatten l = 
  let rec aux acc = function
    | [] -> acc
    | One a :: t -> aux (a::acc) t
    | Many l :: t -> aux (aux acc l) t
  in rev (aux [] l)
;;

(* Test *)
flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
(*- : string list = ["a"; "b"; "c"; "d"; "e"]*)
