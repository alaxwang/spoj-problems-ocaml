open Big_int let b,s=big_int_of_string,string_of_big_int let rec f=function|10->()|n->let y,x=b(read_line()),b(read_line())in let a=div_big_int(add_big_int x y)(big_int_of_int 2)in print_endline(s a);print_endline(s(sub_big_int x a));f(n+1);;f 0