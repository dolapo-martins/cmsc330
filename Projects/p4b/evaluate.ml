(* Part 4b*)
#use "smallc.ml";;

let rec my_pow a b =
	if b = 0 then 1
	else if b = 1 then a
	else a * (my_pow a (b-1))
;;

let getVal x =
	match x with
	| None -> failwith ("variable has no value")
	| Some x -> x
;;

let rec aux env expr = 
	match expr with 
	| Less (x, y) -> if (aux env x) < (aux env y) then 1 else -1
	| Greater (x, y) -> if (aux env x) > (aux env y) then 1 else -1
	| Equal (x, y) -> if (aux env x) == (aux env y) then 1 else -1
	| Sum (x, y) -> (aux env x) + (aux env y)
	| Mult (x, y) -> (aux env x) * (aux env y)
	| Pow (x, y) -> my_pow (aux env x) (aux env y)
	| Id str -> getVal (Hashtbl.find env str)
	| Paren x -> (aux env x)
	| Num x -> x
	| _ -> failwith("something went wrong in aux")
;;

let rec my_eval env lst = 
	match lst with
		| Id str:: t -> let _ = getVal (Hashtbl.find env str) in my_eval env t
		| Assign (Id str, b)::t -> let _ =
			Hashtbl.replace env str (Some (aux env b))
			in my_eval env t
		| Define (Type_Int, Id str)::t -> let _ = 
			if (Hashtbl.mem env str) 
			then failwith("Cannot define variable twice")
			else (Hashtbl.add env str None)
			in my_eval env t
		| Print(exp)::t -> print_string ( string_of_int(aux env exp) ^ "\n")
			; my_eval env t
		| If (exp, if_body, else_body)::t -> 
			if (aux env exp) == 1 then (getexp env if_body)
			else (if else_body != List[] then (getexp env else_body))
			; my_eval env t
		| While (exp, while_body)::t -> 
			if (aux env exp) == 1 
			then ((getexp env while_body) ; my_eval env [(While(exp, while_body))])
			; my_eval env t
		| [] -> ()

and getexp env get_ast = 
	match get_ast with 
	| List expressions -> my_eval env expressions
	| _ -> failwith("You were supposed to start with a List[expressions]")
;;

let eval env param = 
	(match param with
	| Fun(Type_Int, name, args, body) -> getexp env body
	| _ -> failwith("Something went wrong in eval")
	);
env
;;