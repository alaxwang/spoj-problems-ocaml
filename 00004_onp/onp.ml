let print_as_rpn string =
  let (>=:) a b =
    let operators = "(+-*/^" in
    String.index operators a >= String.index operators b in
  let rec iter i stack = match try string.[i] with _ -> ' ' with
      ('a' .. 'z' as operand) ->
        print_char operand; iter (succ i) stack
    | ('+' | '-' | '*' | '/' | '^' as op) ->
      let rec pop_ge stack = match stack with
          hd :: tl when hd >=: op -> print_char hd; pop_ge tl
        | _ -> iter (succ i) (op :: stack) in
      pop_ge stack
    | '(' as c ->
      iter (succ i) (c :: stack)
    | ')' ->
      let rec pop_exp stack = match stack with
          '(' :: tl -> iter (succ i) tl
        | c :: tl -> print_char c; pop_exp tl
        | _ -> failwith "closing bracket without opening" in
      pop_exp stack
    | _ -> print_newline () in
  iter 0 [];;

let rec read i max =
  if i < max then
    (print_as_rpn (read_line ()); read (i + 1) max) in
read 0 (int_of_string (read_line ()));;
