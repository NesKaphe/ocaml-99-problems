(*
Find out whether a list is a palindrome. (easy)
*)

let rev l = 
    let rec aux acc = function
      | [] -> acc
      | a::t -> aux (a::acc) t
    in aux [] l
;;

let is_palindrome l = l = rev l
;;


(* Test *)
is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
(* - : bool = true *)
not (is_palindrome [ "a" ; "b" ]);;
(* - : bool = true *)

