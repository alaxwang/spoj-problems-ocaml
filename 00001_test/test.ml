let rec test s =
  if s <> "42" then
    (print_endline s; test (input_line stdin)) in
test (input_line stdin);;
