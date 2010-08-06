let rev_number n =
  let rec iter reversed n =
    if n = 0 then reversed
    else iter (reversed * 10 + (n mod 10)) (n / 10) in
  iter 0 n;;

let rec iter i max =
  if i < max then
    (print_int (rev_number (Scanf.scanf "%i %i\n" (fun a b -> rev_number a + rev_number b)));
     print_newline ();
     iter (succ i) max) in
iter 0 (read_int ());;
