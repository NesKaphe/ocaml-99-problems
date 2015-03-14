(*

Remove the K'th element from a list. (easy)

*)

let rec remove_at n = function
  | [] -> []
  | a::t -> if n = 0 then t else a :: remove_at (n-1) t
;;

(* Test *)

remove_at 1 ["a";"b";"c";"d"];;
(*- : string list = ["a"; "c"; "d"]*)
