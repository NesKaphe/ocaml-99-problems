(*
Eliminate consecutive duplicates of list elements. (medium)
*)


let compress l = 
  let rec aux last = function
    | [] -> []
    | a::t -> 
      (
	match last with
	| None -> a :: aux (Some a) t
	| Some b -> if b = a then aux (Some a) t else a :: aux (Some a) t
      ) 
  in aux None l
;;

(* Better solution *)
let rec compress2 l = match l with
  | a::b::t -> if a = b then compress2 (b::t) else a:: compress (b::t)
  | _ -> l
;;

(* Test *)
compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
(*- : string list = ["a"; "b"; "c"; "a"; "d"; "e"]*)
compress2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
(*- : string list = ["a"; "b"; "c"; "a"; "d"; "e"]*)
