(* CMSC 330 Organization of Programming Languages 
_____Fall_2016_____ FA2016P3REGEXOCAML
Dr. Anwar Mamat 
Project 3: Regular Expression Interpreter
 
*)

#load "str.cma"

(* ------------------------------------------------- *)
(* MODULE SIGNATURE *)
(* ------------------------------------------------- *)

module type NFA =
  sig
    (* You may NOT change this signature *)

    (* ------------------------------------------------- *)
    (* PART 1: NFA IMPLEMENTATION *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    (* Abstract type for NFAs *)
    type nfa

    (* Type of an NFA transition.

       (s0, Some c, s1) represents a transition from state s0 to state s1
       on character c

       (s0, None, s1) represents an epsilon transition from s0 to s1
     *)
    type transition = int * char option * int

    (* ------------------------------------------------- *)
    (* Returns a new NFA.  make_nfa s fs ts returns an NFA with start
       state s, final states fs, and transitions ts.
     *)
    val make_nfa : int -> int list -> transition list -> nfa

    (* ------------------------------------------------- *)
    (*  Calculates epsilon closure in an NFA.

	e_closure m ss returns a list of states that m could
	be in, starting from any state in ss and making 0 or
	more epsilon transitions.

       There should be no duplicates in the output list of states.
     *)

    val e_closure : nfa -> int list -> int list

    (* ------------------------------------------------- *)
    (*  Calculates move in an NFA.

	move m ss c returns a list of states that m could
	be in, starting from any state in ss and making 1
	transition on c.

       There should be no duplicates in the output list of states.
     *)

    val move : nfa -> int list -> char -> int list

    (* ------------------------------------------------- *)
    (* Returns true if the NFA accepts the string, and false otherwise *)
    val accept : nfa -> string -> bool

    (* ------------------------------------------------- *)
    (* Gives the stats of the NFA

      the first integer representing the number of states
      the second integer representing the number of final states
      the (int * int) list represents the number of states with a particular number of transitions
      e.g. (0,1) means there is 1 state with 0 transitions, (1,2) means there is 2 states with 1 transition
      the list would look something like: [(0,1);(1,2);(2,3);(3,1)]

    *)

    val stats : nfa -> int * int * (int * int) list

    (* ------------------------------------------------- *)
    (* PART 2: REGULAR EXPRESSION IMPLEMENTATION *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    type regexp =
	Empty_String
      | Char of char
      | Union of regexp * regexp
      | Concat of regexp * regexp
      | Star of regexp

    (* ------------------------------------------------- *)
    (* Given a regular expression, print it as a regular expression in
       postfix notation (as in project 2).  Always print the first regexp
       operand first, so output string will always be same for each regexp.
     *)
    val regexp_to_string : regexp -> string 

    (* ------------------------------------------------- *)
    (* Given a regular expression, return an nfa that accepts the same
       language as the regexp
     *)
    val regexp_to_nfa : regexp -> nfa

    (* ------------------------------------------------- *)
    (* PART 3: REGULAR EXPRESSION PARSER *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    (* Given a regular expression as string, parses it and returns the
       equivalent regular expression represented as the type regexp.
     *)
    val string_to_regexp : string -> regexp

    (* ------------------------------------------------- *)
    (* Given a regular expression as string, parses it and returns
       the equivalent nfa
     *)
    val string_to_nfa: string -> nfa

    (* ------------------------------------------------- *)
    (* Throw IllegalExpression expression when regular
       expression syntax is illegal
     *)
    exception IllegalExpression of string

end

(* ------------------------------------------------- *)
(* MODULE IMPLEMENTATION *)
(* ------------------------------------------------- *)

    (* Make all your code changes past this point *)
    (* You may add/delete/reorder code as you wish
       (but note that it still must match the signature above) *)

module NfaImpl =
struct

type transition = int * char option * int

type nfa = int * int list * transition list

let make_nfa ss fs ts = (ss, fs, ts)

let rec e_help trans trans2 ele temp= 
  match trans with
  | [] -> temp
  | (start, ch, dest)::t -> 
      if start = ele && ch = None && (List.mem dest temp = false)
        then let temp = dest::temp in ((e_help trans2 trans2 dest temp) @ (e_help t trans2 ele temp))
      else e_help t trans2 ele temp
;; 


let e_closure m ss = 
  if (List.length ss > 0) then 
    List.sort_uniq compare (ss @ 
      (let (_,_, trans) = m 
      in let temp = []
      in (List.fold_left (fun acc h -> List.sort_uniq compare ((e_help trans trans h temp) @ acc)) [] ss ))
    )
  else []
;;

let rec mv_help m trans ele opt = 
  match trans with 
  | [] -> []
  | (src, ch, dest)::t -> 
      if (src = ele && ch = Some opt) 
        then dest::(mv_help m t ele opt) 
    else mv_help m t ele opt
;;  


let move m ss c = 
    let (start,_, trans) = m 
    in List.sort_uniq compare 
          (List.fold_left (fun acc h -> 
              if c = 'E' 
                then (e_closure m [h]) @ acc 
              else (mv_help m trans h c) @ acc) [] ss)
;;

let rec string_to_list = function
  | "" -> ['E']
  | str -> String.get str 0 ::
          string_to_list (String.sub str 1 ((String.length str) - 1))
;;

let accept m s = 
  let (start, final, trans) = m 
    in let end_states =
      List.fold_left (fun acc h -> e_closure m (move m (e_closure m acc) h)) [start] (string_to_list s)
  in let x = false in
      (List.fold_left(fun acc h -> if (List.mem h end_states) then true else acc) x final)
;; 

let all_states m =
  let (start, final, trans) = m in
    let total_states = List.fold_left(fun acc (src, ch, dest) -> src::dest::acc) [] trans in
      List.sort_uniq compare total_states
;;

let connect_count trans ele =
  List.fold_left(fun acc (src, _, _) -> if src = ele then acc + 1 else acc) 0 trans
;;


let stats n = 
  let (start, final, trans) = n in 
    let connects = List.fold_left (fun acc h -> (connect_count trans h, h)::acc) [] (all_states n)
  in let lst = List.map (fun (src, cnt) -> (src, List.fold_left(fun acc (start, _) -> if start = src then acc + 1 else acc) 0 connects )) connects
in let edge_list = List.sort_uniq compare lst
in (List.length (all_states n), List.length final, edge_list)
;;


type regexp =
	  Empty_String
	| Char of char
	| Union of regexp * regexp
	| Concat of regexp * regexp
	| Star of regexp
;;


let rec regexp_to_string r = 
  match r with 
  | Empty_String -> "E"
  | Char x -> String.make 1 x
  | Union (x, y) -> String.concat " " [(regexp_to_string x); (regexp_to_string y); "|"]
  | Concat (x, y) -> String.concat " " [(regexp_to_string x); (regexp_to_string y); "."]
  | Star x -> String.concat " " [(regexp_to_string x); "*"]
;;

let next = 
  let count = ref 0 in 
    function () ->
      let temp = !count in 
        count := (!count) + 1;
        temp
;;

let rec trans_for_list dest lst = 
  match lst with
  |[] -> []
  | h::t -> (h, None, dest)::(trans_for_list dest t)
;;

let rec regexp_to_nfa r = 
  match r with
  | Empty_String -> let x = next() in make_nfa x [x] []
  | Char ch -> let x = next() in let y = next() in make_nfa x [y] [(x, Some ch, y)]
  | Union (m,n) ->
    let (asrc, aend, atrans) = (regexp_to_nfa m) in let (bsrc, bend, btrans) = (regexp_to_nfa n) in let x = next() in let y = next() in 
      make_nfa x [y] (([(x, None, asrc); (x, None, bsrc)] @ ((atrans @ btrans) @ (trans_for_list y aend))) @ (trans_for_list y bend))
  | Concat (m,n) ->  let (asrc, aend, atrans) = (regexp_to_nfa m) in let (bsrc, bend, btrans) = (regexp_to_nfa n) in make_nfa asrc bend ((btrans @ atrans) @ (trans_for_list bsrc aend))
  | Star ch -> let (asrc, aend, atrans) = (regexp_to_nfa ch) in let x = next() in let y = next() in 
    make_nfa x [y] (([(x, None, asrc); (x, None, y); (y, None, x)] @ atrans) @ trans_for_list y aend)
;;

exception IllegalExpression of string

(************************************************************************)
(* PARSER. You shouldn't have to change anything below this point *)
(************************************************************************)

(* Scanner code provided to turn string into a list of tokens *)

type token =
   Tok_Char of char
 | Tok_Epsilon
 | Tok_Union
 | Tok_Star
 | Tok_LParen
 | Tok_RParen
 | Tok_END

let re_var = Str.regexp "[a-z]"
let re_epsilon = Str.regexp "E"
let re_union = Str.regexp "|"
let re_star = Str.regexp "*"
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"

let tokenize str =
 let rec tok pos s =
   if pos >= String.length s then
     [Tok_END]
   else begin
     if (Str.string_match re_var s pos) then
       let token = Str.matched_string s in
       (Tok_Char token.[0])::(tok (pos+1) s)
	 else if (Str.string_match re_epsilon s pos) then
       Tok_Epsilon::(tok (pos+1) s)
	 else if (Str.string_match re_union s pos) then
       Tok_Union::(tok (pos+1) s)
	 else if (Str.string_match re_star s pos) then
       Tok_Star::(tok (pos+1) s)
     else if (Str.string_match re_lparen s pos) then
       Tok_LParen::(tok (pos+1) s)
     else if (Str.string_match re_rparen s pos) then
       Tok_RParen::(tok (pos+1) s)
     else
       raise (IllegalExpression "tokenize")
   end
 in
 tok 0 str

(*
  A regular expression parser. It parses strings matching the
  context free grammar below.

   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let lookahead tok_list = match tok_list with
	[] -> raise (IllegalExpression "lookahead")
	| (h::t) -> (h,t)

let rec parse_S l =
	let (a1,l1) = parse_A l in
	let (t,n) = lookahead l1 in
	match t with
		Tok_Union -> (
		let (a2,l2) = (parse_S n) in
		(Union (a1,a2),l2)
		)
		| _ -> (a1,l1)

and parse_A l =
	let (a1,l1) = parse_B l in
	let (t,n) = lookahead l1 in
	match t with
	Tok_Char c ->
		let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
	| Tok_Epsilon ->
		let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
	| Tok_LParen ->
		let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
	| _ -> (a1,l1)

and parse_B l =
	let (a1,l1) = parse_C l in
	let (t,n) = lookahead l1 in
	match t with
	Tok_Star -> (Star a1,n)
	| _ -> (a1,l1)

and parse_C l =
	let (t,n) = lookahead l in
	match t with
   	  Tok_Char c -> (Char c, n)
	| Tok_Epsilon -> (Empty_String, n)
	| Tok_LParen ->
		let (a1,l1) = parse_S n in
		let (t2,n2) = lookahead l1 in
		if (t2 = Tok_RParen) then
			(a1,n2)
		else
			raise (IllegalExpression "parse_C 1")
	| _ -> raise (IllegalExpression "parse_C 2")

let string_to_regexp str =
	let tok_list = tokenize str in
	let (a,t) = (parse_S tok_list) in
	match t with
	[Tok_END] -> a
	| _ -> raise (IllegalExpression "string_to_regexp")

let string_to_nfa s = regexp_to_nfa (string_to_regexp s)

end

module Nfa : NFA = NfaImpl;;
