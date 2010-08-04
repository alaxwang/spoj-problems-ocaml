open Nativeint;;

let ( / ) x y = div x y;;
let ( * ) x y = mul x y;;

let powers =
  let rec iter list i =
    if i > 1000000000n then list
    else iter (i :: list) (i * 5n) in
  List.rev (iter [] 5n);;

let get_zeroes x =
  let rec iter num powers =
    match powers with
        hd :: tl when hd <= x -> iter (num + to_int (x / hd)) tl
      | _ -> num in
  iter 0 powers;;

let rec iter i max =
  if i < max then
    let number = of_int (read_int ()) in
    (print_int (get_zeroes number); print_newline (); iter (succ i) max) in
iter zero (of_string (input_line stdin));;
