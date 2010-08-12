(* using KMP here *)

let print_indices needle =
  let table =
    let length = String.length needle in
    let array = Array.make (succ length) ~-1 in
    let rec iter i j =
      if i >= length then array
      else
        let rec next j =
          if j >= 0 && needle.[i] <> needle.[j] then next array.(j)
          else succ j in
        let i, j = succ i, next j in
        array.(i) <- j;
        iter i j in
    iter 0 ~-1 in
  let n_len = String.length needle in
  let rec iter i j = match try Some (input_char stdin) with End_of_file -> None with
      Some '\r' | Some '\n' | None -> ()
    | Some c ->
      let rec next j =
        if j >= 0 && c <> needle.[j] then next table.(j)
        else succ j in
      let i, j = succ i, next j in
      if j = n_len then (Printf.printf "%i\n" (i - j); iter i table.(j))
      else iter i j in
  iter 0 0;;

let rec iter first = match try Some (int_of_string (read_line ())) with End_of_file -> None with
    Some _ -> if not first then print_newline (); print_indices (read_line ()); iter false
  | None -> () in
iter true;;
