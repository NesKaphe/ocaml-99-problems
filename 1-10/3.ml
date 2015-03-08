(*
Find the k'th element of a list. (easy)
*)


let rec at n l = match l with
  | [] -> None
  | a::t -> if n = 1 then Some a else at (n-1) t
;;

(* Test *)
at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
(* - : string option = Some "c" *)
at 3 [ "a" ];;
(* - : string option = None *)
