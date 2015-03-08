(* 
Write a function last : 'a list -> 'a option that returns the last element
of a list. (easy)
*)

let rec last = function
  | [] -> None
  | [x] -> Some x
  | h::t -> last t
;;

(* Tests *)
last [ "a" ; "b" ; "c" ; "d" ];;
(* - : string option = Some "d" *)
last [];;
(* - : 'a option = None*)
