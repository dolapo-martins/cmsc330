(* Dolapo A. Martins *)
(* Lab 4 *)


let rec map f l = 
	match l with 
	| [] -> []
	| h::t -> (f h)::(map f t)
;;

let rec fold f acc l = 
	match l with
	| [] -> acc
	| h::t -> fold f (f acc h) t
;;

let double x = 2 * x;;

map double [1;2;3];;

map (fun x -> x * 2) [1;2;3];;

let evens l = 
	map (fun x -> (x mod 2 = 0)) l
;;

let summation l = 
	fold (fun acc x -> x + acc) 0 l
;;

let sum_cube_odd l n =
	fold (fun acc x -> (if (x mod 2 != 0) && (x <= n) then (x * x * x) + acc else 0)) 0 l
;;


let mean_of_lists l = 
	let length l = fold (fun acc h -> acc + 1) 0 l in 
	let len = length l in
	let list_of_sums = 
		map ( fun x -> (fold (fun acc h -> acc + h) 0 x) list_of_sums
		in
	let sum_of_sums = () in
	sum_of_sums / len;;
