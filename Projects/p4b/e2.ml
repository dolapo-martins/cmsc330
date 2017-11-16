(* Part 4b*)
#use "smallc.ml";;

let eval env ast = 
	let rec aux tree = 
		match tree with
		| Num val:: val; eval env t
		| (Id str)::t -> 
			let x = Hashtbl.find env str in 
				(match x with
				| None -> failwith("value not assigned")
				| Some val -> val
				)
		| (Assign (Id str, b))::t -> Hashtbl.replace env str (Some (aux b)); eval env t
		| (Define (Type_Int, Id str))::t -> 
			if (Hashtbl.mem env str) 
			then failwith("Cannot define variable twice")
			else (Hashtbl.add env str None); eval env t
		| (Sum (a,b))::t -> ((aux a) + (aux b)); eval env t
		| (Mult (a, b))::t -> ((aux a) * (aux b)); eval env t
		| (Pow (a, b))::t -> (my_pow (aux a) (aux b)); eval env t
		| (Less (a, b))::t -> ((aux a) < (aux b)); eval env t
		| (Greater (a, b))::t -> ((aux a) > (aux b)); eval env t
		| (Equal (a, b))::t -> ((aux a) = (aux b)); eval env t
		| (Paren x)::t -> (aux x); eval env t
		| (If (exp, stmt, elsbranch))::t -> 
			if (aux exp) then (aux stmt); eval env t else
				(if elsbranch != List[] then (aux elsbranch); eval env t)
		| (While (exp, stmt))::t -> if (aux exp) then (aux stmt); eval env While(exp, stmt) else eval env t
	in let my_ast = match ast with
	| List x -> x 
	(*| _ -> failwith("ast doesn't match") in *)
	in aux my_ast;
env
;;

let rec my_pow a b =
	if b = 0 then 1
	else if b = 1 then a
	else a * (my_pow a (b-1))
;;
