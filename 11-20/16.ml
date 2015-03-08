(*
Drop every N'th element from a list. (medium)
*)

let drop l n = 
  let rec aux count = function
    | [] -> []
    | a::t -> if count = n then aux 1 t else a :: aux (count+1) t
  in aux 1 l
;;

(* Test *)
drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
(*- : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]*)
