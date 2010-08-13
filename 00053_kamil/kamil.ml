for k=0 to 9 do let rec i c=match input_char stdin with 'T'|'D'|'F'|'L'->i(c*2)|'\n'->Printf.printf "%i\n" c|_->i c in i 1 done
