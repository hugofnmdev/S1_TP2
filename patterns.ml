(* 1. - PATTERNS
1.1 - Must-do
1.1.1 - Build me a line *)

let rec build_line n str = match n with
|1 -> str
|_ -> str ^ build_line (n-1) str;;

(* 1.1.2 - Draw me a square *)

let rec square n str = match n with
|1 -> print_string str
|_ -> print_string (build_line n str); print_char '\n'; square n str;;

(* 1.1.3 - Draw me a square - bis *)

let square2 n (str,str2) = 
let line n (str,str2) = match n with
|1 -> str
|_ -> str

(* 1.1.4 - Draw me a triangle *)

let rec triangle n str = match n with
|1 -> print_string str
|_ -> triangle (n-1) str; print_char '\n'; print_string (build_line n str);;

(* 1.2 - Bonus
1.2.1 - Draw me a pyramid *)

let pyramid n (str,str2) = match n with
|1 -> print_string str
|_ -> 
