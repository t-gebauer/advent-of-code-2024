let part1 _lines = "TODO"
let part2 _lines = "TODO"

let example = Lib.parse_lines {|
1 2 3 4
|}

let%expect_test _ =
  print_string (part1 example);
  [%expect {| TODO |}]

let%expect_test _ =
  print_string (part2 example);
  [%expect {| TODO |}]
