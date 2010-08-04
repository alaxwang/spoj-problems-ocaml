open Num;;

let factorial n =
  let n = Int n in
  let rec iter i fact =
    if i = n then i */ fact
    else iter (succ_num i) (i */ fact) in
  iter (Int 1) (Int 1);;

let max = read_int () in
let rec iter i =
  if i < max then
    (print_endline (string_of_num (factorial (read_int ()))); iter (succ i)) in
iter 0;;
