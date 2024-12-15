let valid_equation operators numbers =
  let test_value = List.hd numbers in
  let numbers = List.tl numbers in
  let rec f cur nums =
    match nums with
    | [] -> cur = test_value
    | x :: xs ->
        Option.is_some (List.find_opt (fun op -> f (op cur x) xs) operators)
  in
  f (List.hd numbers) (List.tl numbers)

let part1 lines =
  lines
  |> List.map Lib.extract_numbers
  |> List.filter (valid_equation [ Int.add; Int.mul ])
  |> List.map List.hd |> Lib.sum |> string_of_int

let concatenation a b = int_of_string (string_of_int a ^ string_of_int b)

let part2 lines =
  lines
  |> List.map Lib.extract_numbers
  |> List.filter (valid_equation [ Int.add; Int.mul; concatenation ])
  |> List.map List.hd |> Lib.sum |> string_of_int

let example =
  Lib.parse_lines
    {|
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
|}

let%expect_test _ =
  print_string (part1 example);
  [%expect {| 3749 |}]

let%expect_test _ =
  print_string (part2 example);
  [%expect {| 11387 |}]
