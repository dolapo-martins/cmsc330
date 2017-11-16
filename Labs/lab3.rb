#Exercise 1
r = /^(\d{3})-(\d{3})-(\d{4})$/
"808-355-5585" =~ r
puts $1
puts $2
puts $3

r = /^(([^aeiouAEIOU]*[aeiuoAEIOU]){2}[^aeiouAEIOU]*)$/
"dog" =~ r
"doggo" =~ r
"I match!" =~
"I do not" =~ r

r = /^((0.*(010).*1)|(1.*(010).*0))$/
"00101" =~ r
"10011110110100" =~ r

#Exercise 2 - OCaml
let sqrt a = 
	sqrt(a) ;;
	
let rec fibonacci n = 
	if n < 2 then n
	else fibonacci (n - 1) + fibonacci (n - 2);;