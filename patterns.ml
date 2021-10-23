(* 1. - PATTERNS
1.1 - Must-do
1.1.1 - Build me a line *)

let rec build_line n str = match n with
|1 -> str
|_ -> str ^ build_line (n-1) str;;

(* 1.1.2 - Draw me a square *)

let square n str =
  if n < 0 then invalid_arg "square: n must be a natural"
  else (
    let rec print_line = function
        0 -> print_string ""
      | 1 -> (
        print_string (build_line n str) ;
        print_newline()
      )
      | x -> (
        print_string (build_line n str) ;
        print_newline() ;
        print_line (x - 1)
      )
    in print_line n
  )
;;


(* 1.1.3 - Draw me a square - bis *)

let square2 n (str1, str2) =
  if n < 0 then invalid_arg "square2: n must be a natural"
  else (
    let rec print_line x =
      let str =
        if x mod 2 = 1 then str1 ^ str2
        else str2 ^ str1
      in match x with
           0 -> print_string ""
         | 1 -> (
           print_string (build_line n str) ;
           print_newline()
         )
         | x -> (
           print_string (build_line n str) ;
           print_newline() ;
           print_line (x - 1)
         )
    in print_line n
  )
;;

(* 1.1.4 - Draw me a triangle *)

let rec triangle n str = match n with
|1 -> print_string str
|_ -> triangle (n-1) str; print_char '\n'; print_string (build_line n str);;

(* 1.2 - Bonus
1.2.1 - Draw me a pyramid *)

let pyramid n (str1, str2) =
  if n < 0 then invalid_arg "pyramid: n must be natural"
  else (
    let rec print_line = function
        0 -> print_string ""
      | x when x = 1 -> (
        print_string (build_line (n * 2) str2) ;
        print_newline()
      )
      | x -> (
        let n1 = x - 1 and n2 = n - (x - 1)
        in (
            print_string (
                (build_line n1 str1) ^
                  (build_line n2 str2) ^
                    (build_line n2 str2) ^
                      (build_line n1 str1)
              ) ;
            print_newline() ;
            print_line (x - 1)
          )
      )
    in print_line n
  )
;;

(* 1.2.2 - Cross *)

let cross n (str1, str2) =
  if n <= 0 then invalid_arg "cross: n must be positive"
  else (
  let rec print_line x =
       let line_part_one = build_line (x - 1) str1
       in match x with
            0 -> print_string ""
          | x when x = n -> (
            print_string (line_part_one ^ str2 ^ line_part_one) ;
            print_newline()
          )
          | x -> (
            let line = line_part_one ^ str2 ^ build_line ((2 * n) - 1 - (2 * x)) str1 ^ str2 ^ line_part_one
            in print_string line ;
               print_newline() ;
               print_line (x + 1) ;
               print_string line ;
               print_newline()
          )
  in print_line 1
)
;;
