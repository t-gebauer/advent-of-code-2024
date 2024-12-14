let part1 _lines = 42 |> string_of_int
let part2 _lines = 1337 |> string_of_int

let example = Lib.parse_lines {|
1 2 3 4
|}

let%expect_test _ =
  print_string (part1 example);
  [%expect {| 42 |}]

let%expect_test _ =
  print_string (part2 example);
  [%expect {| 1337 |}]
