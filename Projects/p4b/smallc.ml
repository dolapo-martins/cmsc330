(*  
	CMSC330 Fall 2016
	This ocaml code reads a C code and properly indents it
	
	compile for debug:
		ocamlc -g Str.cma smallc.ml 
	
	@author: Anwar Mamat
	@date: 10/15/2016
*)

#load "str.cma"

type data_type =
	|Type_Int
;;

(* Use this as your abstract syntax tree *)

type ast =
  | Id of string
  | Num of int
  | Define of data_type * ast
  | Assign of ast * ast
  | List of ast list
  | Fun of data_type * string * ast * ast   (* return type * function name * argument list * statement list *)
  | Sum of ast * ast
  | Greater of ast * ast
  | Equal of ast * ast
  | Less of ast * ast
  | Mult of ast * ast
  | Pow of  ast * ast
  | Print of ast
  | If of ast * ast * ast	(* cond * if brach * else branch *)
  | While of ast * ast
  | Paren of ast
  
;;

type token =
 | Tok_Id of string
 | Tok_Num of int
 | Tok_String of string
 | Tok_Assign
 | Tok_Greater
 | Tok_Less
 | Tok_Equal
 | Tok_LParen
 | Tok_RParen
 | Tok_Semi
 | Tok_Main
 | Tok_LBrace
 | Tok_RBrace
 | Tok_Int 
 | Tok_Float
 | Tok_Sum
 | Tok_Mult
 | Tok_Pow
 | Tok_Print
 | Tok_If
 | Tok_Else
 | Tok_While
 | Tok_END
 
(* tokens *)
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_lbrace = Str.regexp "{"
let re_rbrace = Str.regexp "}"
let re_assign = Str.regexp "="
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_equal = Str.regexp "=="
let re_semi = Str.regexp ";"
let re_int = Str.regexp "int"
let re_float = Str.regexp "float"
let re_printf = Str.regexp "printf"
let re_main = Str.regexp "main"
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_num = Str.regexp "[-]?[0-9]+"
let re_string = Str.regexp "\"[^\"]*\""
let re_whitespace = Str.regexp "[ \t\n]"
let re_add = Str.regexp "+"
let re_mult = Str.regexp "*"
let re_pow = Str.regexp "\\^"
let re_if = Str.regexp "if"
let re_else = Str.regexp "else"
let re_while = Str.regexp "while"


exception Lex_error of int
exception Parse_error of int ;;
exception IllegalExpression of string

let tokenize s =
 let rec tokenize' pos s =
   if pos >= String.length s then
     [Tok_END]
   else begin
     if (Str.string_match re_lparen s pos) then
       Tok_LParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_rparen s pos) then
       Tok_RParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_add s pos) then
       Tok_Sum::(tokenize' (pos+1) s)
     else if (Str.string_match re_mult s pos) then
       Tok_Mult::(tokenize' (pos+1) s)
     else if (Str.string_match re_equal s pos) then
       Tok_Equal::(tokenize' (pos+2) s)
     else if (Str.string_match re_if s pos) then
       Tok_If::(tokenize' (pos+2) s)
     else if (Str.string_match re_else s pos) then
       Tok_Else::(tokenize' (pos+4) s)    
     else if (Str.string_match re_while s pos) then
       Tok_While::(tokenize' (pos+5) s)       
	else if (Str.string_match re_pow s pos) then
       Tok_Pow::(tokenize' (pos+1) s)
    else if (Str.string_match re_printf s pos) then
       Tok_Print::tokenize' (pos+6) s
    else if (Str.string_match re_lbrace s pos) then
       Tok_LBrace::(tokenize' (pos+1) s)
    else if (Str.string_match re_rbrace s pos) then
       Tok_RBrace::(tokenize' (pos+1) s)
    else if (Str.string_match re_assign s pos) then
       Tok_Assign::(tokenize' (pos+1) s)
    else if (Str.string_match re_greater s pos) then
       Tok_Greater::(tokenize' (pos+1) s)
    else if (Str.string_match re_less s pos) then
       Tok_Less::(tokenize' (pos+1) s)
    else if (Str.string_match re_semi s pos) then
       Tok_Semi::(tokenize' (pos+1) s)
    else if (Str.string_match re_int s pos) then
       Tok_Int::(tokenize' (pos+3) s)
    else if (Str.string_match re_float s pos) then
       Tok_Float::(tokenize' (pos+5) s)
    else if (Str.string_match re_main s pos) then
       Tok_Main::(tokenize' (pos+4) s)
     else if (Str.string_match re_id s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Id token)::(tokenize' new_pos s)
     else if (Str.string_match re_string s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       let tok = Tok_String (String.sub token 1 ((String.length token)-2)) in
       tok::(tokenize' new_pos s)
     else if (Str.string_match re_num s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Num (int_of_string token))::(tokenize' new_pos s)
     else if (Str.string_match re_whitespace s pos) then
       tokenize' (Str.match_end ()) s
     else
       raise (Lex_error pos)
   end
 in
 tokenize' 0 s
 
 
 (* C Grammar *)
 (* 
 
 basicType-> 'int'
  mainMethod-> basicType 'main' '(' ')' '{' methodBody '}'
  methodBody->(localDeclaration | statement)*
  localDeclaration->basicType ID ';'
  statement->
    whileStatement
    |ifStatement
    |assignStatement
    |printStatement
  
  assignStatement->ID '=' exp ';'
  ifStatement -> 'if' '(' exp ')'  '{' ( statement)* '}'  ( 'else' '{'( statement)* '}')?
  whileStatement -> 'while''(' exp ')' '{'(statement )*'}'
  printStatement->'printf' '(' exp ')' ';'
  exp -> additiveExp (('>'  | '<'  | '==' ) additiveExp )*
  additiveExp -> multiplicativeExp ('+' multiplicativeExp)*
  multiplicativeExp-> powerExp ( '*' powerExp  )*
  powerExp->primaryExp ( '^' primaryExp) *
  primaryExp->'(' exp ')' | ID 
  ID->( 'a'..'z' | 'A'..'Z') ( 'a'..'z' | 'A'..'Z' | '0'..'9')*
  WS-> (' '|'\r'|'\t'|'\n') 



*)

(*----------------------------------------------------------
  function lookahead : token list -> (token * token list)
	Returns tuple of head of token list & tail of token list
*)

let lookahead tok_list = match tok_list with
        [] -> raise (IllegalExpression "lookahead")
        | (h::t) -> (h,t)
;;


let tok_to_str t = ( match t with
          Tok_Num v -> string_of_int v
        | Tok_Sum -> "+"
        | Tok_Mult ->  "*"
        | Tok_LParen -> "("
        | Tok_RParen -> ")"
		| Tok_Pow->"^"
        | Tok_END -> "END"
        | Tok_Id id->id
		| Tok_String s->s
		| Tok_Assign->"="
		 | Tok_Greater->">"
		 | Tok_Less->"<"
		 | Tok_Equal->"=="
		 | Tok_Semi->";"
		 | Tok_Main->"main"
		 | Tok_LBrace->"{"
		 | Tok_RBrace->"}"
		 | Tok_Int->"int" 
		 | Tok_Float->"float"
		 | Tok_Print->"printf"
		 | Tok_If->"if"
		 | Tok_Else->"else"
		 | Tok_While-> "while"
    )

let print_token_list tokens =
	print_string "Input token list = " ;
	List.iter (fun x -> print_string (" " ^ (tok_to_str x))) tokens;
	print_endline ""
;;
        

(* -------------- Your Code Here ----------------------- *)
let rec parse_Function lst = 
	match lst with 
	| [] -> raise (IllegalExpression "parse_Function")
	| Tok_Int::Tok_Main::Tok_LParen::Tok_RParen::Tok_LBrace::t -> 
		let (rest, whatev) = parse_body t in 
			(match whatev with
			| [] -> failwith ("You did not end with enough tokens in parse_Function")
			| r::h::[] -> 
				if r = Tok_RBrace && h = Tok_END then (Fun (Type_Int, "main", List[], rest), [])
				else failwith ("You did not end with a } in parse_Function")
			| _ -> failwith("Something went wrong in parse_Function")
			)
	| _ -> failwith("Invaild input to parse_Function")

and parse_body (lst:token list) : (ast * token list) = 
	let (marker, rest) = lookahead lst in
		match marker with
		| Tok_Int -> let (a, b) = parse_local lst in let (c, d) = parse_body b in 
				(match c with
				| List c -> (List(a::c), d)
				| _ -> failwith("You did not return a list tok_int/parse_body")
				)
		| Tok_While -> let (a, b) = parse_while lst in let (c, d) = parse_body b in
				(match c with
				| List c -> (List(a::c), d)
				| _ -> failwith("You did not return a list tok_while/parse_body")
				)
		| Tok_If -> let (a, b) = parse_if lst in let (c, d) = parse_body b in
				(match c with
				| List c -> (List(a::c), d)
				| _ -> failwith("You did not return a list tok_if/parse_body")
				)
		| Tok_Id a -> let (a, b) = parse_assign lst in let (c, d) = parse_body b in
				(match c with
				| List c -> (List(a::c), d)
				| _ -> failwith("You did not return a list tok_id/parse_body")
				)
		| Tok_Print -> let (a, b) = parse_print lst in let (c, d) = parse_body b in
				(match c with
				| List c -> (List(a::c), d)
				| _ -> failwith("You did not return a list tok_print/parse_body")
				)
		| Tok_RBrace -> (List[], Tok_RBrace::rest)
		| _ -> failwith("something went wrong in parse_body")

and parse_local lst = 
	match lst with 
	| Tok_Int::(Tok_Id c)::tok::t -> 
		if tok = Tok_Semi then let x = Define(Type_Int, Id c) in (x, t)
		else failwith ("variable declaration not terminated by ;")
	| _ -> failwith("Your local declaration is not formated properly in parse_local")

and parse_expression lst = 
	let (exp1, toks1) = parse_additive lst in
	let (op, rest) = lookahead toks1 in 
	match op with
	| Tok_Less -> 
		let (exp2, toks2) = parse_expression rest in (Less(exp1, exp2), toks2)
	| Tok_Greater ->
		let (exp2, toks2) = parse_expression rest in (Greater(exp1, exp2), toks2)
	| Tok_Equal ->
		let (exp2, toks2) = parse_expression rest in (Equal(exp1, exp2), toks2)
	| _ -> (exp1, toks1)

and parse_additive lst = 
	let (exp1, toks1) = parse_multiplicative lst in
	let (op, rest) = lookahead toks1 in 
	match op with
	| Tok_Sum -> 
		let (exp2, toks2) = parse_additive rest in (Sum(exp1, exp2), toks2)
	| _ -> (exp1, toks1)

and parse_multiplicative lst = 
	let (exp1, toks1) = parse_power lst in
	let (op, rest) = lookahead toks1 in 
	match op with
	| Tok_Mult -> 
		let (exp2, toks2) = parse_multiplicative rest in (Mult(exp1, exp2), toks2)
	| _ -> (exp1, toks1)

and parse_power lst = 
	let (exp1, toks1) = parse_primary lst in
	let (op, rest) = lookahead toks1 in 
	match op with
	| Tok_Pow -> 
		let (exp2, toks2) = parse_power rest in (Pow(exp1, exp2), toks2)
	| _ -> (exp1, toks1)

and parse_primary lst = 
	match lst with
	| Tok_LParen::t -> let (a, b) = parse_expression t in 
		(match b with
		| Tok_RParen::t -> (Paren (a), t)
		| _ -> failwith ("You did not have a right paren with your expression")
		)
	| Tok_Id x::t -> (Id (x), t)
	| Tok_Num x::t -> (Num (x),t)
	| _ -> failwith("Invalid input to parse_primary")

and parse_while lst = 
	match lst with
	| Tok_While::Tok_LParen::t-> let (exp, b) = parse_expression t in 
		(match b with
		 | Tok_RParen::Tok_LBrace::t -> let (stmt, d) = parse_body t in
		 	(match d with 
		 	| Tok_RBrace::t -> (While(exp, stmt), t)
		 	| _ -> failwith ("You did not have a matching right brace in your while loop") 
		 	)
		 | _ -> failwith("Your parse_while does not get to statement properly")
		)
	| _ -> failwith("Incorrect parse_while syntax")

and parse_if lst = 
	match lst with 
	| Tok_If::Tok_LParen::et -> let (exp, expToks) = parse_expression et in
		(match expToks with 
		| Tok_RParen::Tok_LBrace::st -> let (stmt, stmtToks) = parse_body st in 
			(match stmtToks with
			| Tok_RBrace::elst -> let (els, rest) = lookahead elst in 
				(match els with
				| Tok_Else -> let (lBrace, h) = lookahead rest in let (stmt2, stmt2Toks) = parse_body h in
					(match stmt2Toks with
					| Tok_RBrace::st2 -> (If(exp, stmt, stmt2), st2)
					| _ -> failwith ("You did not have a matching right brace in your else condition")
					)
				| _ -> (If(exp, stmt, List[]), elst)
				)
			|_ -> failwith("Where is your right brace? parse_if")
			)
		| _ -> failwith("You did not close the expression block properly in parse_if")
		)
	| _ -> failwith ("Something went wrong in parse_if")

and parse_assign lst =
	match lst with
	| Tok_Id id::Tok_Assign::t -> let (exp, semi) = parse_expression t in
		(match semi with 
		| Tok_Semi::rest -> (Assign(Id(id), exp), rest)
		| _ -> failwith ("No terminating semicolon in parse_assign")
		)
	| _ -> failwith("Error in parse_assign")

and parse_print lst = 
	match lst with
	| Tok_Print::Tok_LParen::t -> let (exp, toks) = parse_expression t in
		(match toks with
		| Tok_RParen::Tok_Semi::t -> (Print(exp), t)
		| _ -> failwith("No matching rParen or semicolon in parse_print")
		)
	| _ -> failwith("Error in parse_print")
;;

exception Error of int ;;

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


(* -------------- Your Code Here ----------------------- *)

let rec pretty_print pos x =
	match x with
	| Fun (Type_Int, name, args, body) -> print_string ("int " ^ name ^ "(){\n" ^ (pretty_body (pos + 4) body) ^ "}\n")
	| _ -> failwith("Not the right declaration for the function")

and pretty_body pos body = 
	match body with
	| List c -> pretty_bodyList pos c
	| _ -> failwith ("Something went so wrong -> expected body to be a List[]")

and	pretty_bodyList pos lst =
	let underScores = (String.make pos '_') in 
		match lst with
		| Define(Type_Int, Id var)::rest_main -> 
			underScores ^ "int " ^ var ^ ";\n" ^ (pretty_bodyList pos rest_main)
		| While(cond, while_body)::rest_main -> 
			underScores ^ "while(" ^ pretty_expression cond ^ "){\n" 
			^ pretty_body (pos + 4) while_body ^ underScores ^ "}\n" 
			^ (pretty_bodyList pos rest_main)
		| Assign(Id var, exp)::rest_main -> 
			underScores ^ var ^ " = " ^ pretty_expression exp ^ ";\n" 
			^ (pretty_bodyList pos rest_main)
		| If(cond, if_body, else_body)::rest_main -> 
			let else_string = if (else_body = List[]) then "\n" 
				else "else{\n" ^ (pretty_body (pos + 4) else_body) ^ underScores ^ "}\n" in 
			underScores ^ "if(" ^ pretty_expression cond ^ "){\n" ^ (pretty_body (pos + 4) if_body) 
			^  underScores ^ "}" ^ else_string ^ (pretty_bodyList pos rest_main)
		| Print(exp)::rest_main -> 
			underScores ^ "printf(" ^ pretty_expression exp ^ ");\n" ^ (pretty_bodyList pos rest_main)
		| Id var::rest_main -> underScores ^ var ^ ";\n" ^ pretty_bodyList pos rest_main
		| _ -> ""

and pretty_expression cond =
	match cond with 
	| Less (x,y) -> pretty_expression x ^ " < " ^ pretty_expression y
	| Greater (x,y) -> pretty_expression x ^ " > " ^ pretty_expression y
	| Equal (x,y) -> pretty_expression x ^ " == " ^ pretty_expression y
	| Sum (x, y) -> pretty_expression x ^ " + " ^ pretty_expression y
	| Mult (x, y) -> pretty_expression x ^ " * " ^ pretty_expression y
	| Pow (x, y) -> pretty_expression x ^ " ^ " ^ pretty_expression y
	| Id var -> "" ^ var
	| Paren x -> "(" ^ pretty_expression x ^ ")"
	| Num x -> "" ^ string_of_int x
	| _ -> failwith("something went wrong in pretty_expression")
;;


(* ----------------------------------------------------- *)


(*
you can test your parser and pretty_print with following code 
*)

(*

let prg1 = read_lines "main.c";;
let code = List.fold_left (fun x y->x^y) "" prg1;;	
let t = tokenize code;;
let (a,b)=parse_Function t;;

*)