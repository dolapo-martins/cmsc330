let is_empty bst = 
	if bst = Nil then true else false
;;

 let rec insert bst a = 
 	match bst with 
 	| Nil -> Node (a, Nil, Nil)
 	| Node (v, l, r) when a > v -> Node (v, l, insert r a)
 	| Node (v, l, r) when a < v -> Node (v, insert l a, r)
 	| Node (v, l, r) -> bst
;;