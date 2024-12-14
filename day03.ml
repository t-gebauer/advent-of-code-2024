type instruction = Mul of int * int | Do of bool
[@@deriving show { with_path = false }]

(* Str.regexp is rather limited: no \d, no {}; and only grouping parentheses need to be escaped *)
let mul_regex =
  Str.regexp
    {|do()\|don't()\|mul(\([1-9][0-9]?[0-9]?\),\([1-9]?[0-9]?[0-9]?\))|}

let%expect_test _ =
  let t s = Str.string_match mul_regex s 0 |> Printf.printf "%b " in
  ignore (List.map t [ "mul(1,2)"; "mul(44,46)"; "mul(123,4)"; "mul(4,123)" ]);
  [%expect {| true true true true |}];
  ignore
    (List.map t
       [
         "mul(4*,2)";
         "mul(6,9!)";
         "?(12,34)";
         "mul(2 ,4)";
         "mul (2,4)";
         "mul(010,0)";
       ]);
  [%expect {| false false false false false false |}]

let match_instructions line =
  let extract substring =
    match substring with
    | "do()" -> Do true
    | "don't()" -> Do false
    | _ ->
        Mul
          ( Str.matched_group 1 line |> int_of_string,
            Str.matched_group 2 line |> int_of_string )
  in
  Lib.regex_match_all_map mul_regex extract line

let process ?(ignoreDo = true) instructions =
  let rec p' result enabled instrs =
    match instrs with
    | [] -> result
    | inst :: rest -> (
        match inst with
        | Do enabled -> p' result enabled rest
        | Mul (a, b) ->
            let result =
              if enabled || ignoreDo then result + (a * b) else result
            in
            p' result enabled rest)
  in
  p' 0 true instructions

let part1 lines =
  lines |> String.concat "" |> match_instructions |> process |> string_of_int

let example1 =
  [ "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" ]

let%expect_test _ =
  match_instructions (List.nth example1 0)
  |> List.map (fun instr -> Printf.printf "%s " (show_instruction instr))
  |> ignore;
  [%expect {| (Mul (2, 4)) (Mul (5, 5)) (Mul (11, 8)) (Mul (8, 5)) |}]

let%expect_test "part1" =
  print_string (part1 example1);
  [%expect {| 161 |}]

let example2 =
  [
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";
  ]

let%expect_test _ =
  match_instructions (List.nth example2 0)
  |> List.map (fun instr -> Printf.printf "%s " (show_instruction instr))
  |> ignore;
  [%expect
    {| (Mul (2, 4)) (Do false) (Mul (5, 5)) (Mul (11, 8)) (Do true) (Mul (8, 5)) |}]

let part2 lines =
  lines |> String.concat "" |> match_instructions |> process ~ignoreDo:false
  |> string_of_int

let%expect_test _ =
  print_string (part2 example2);
  [%expect {| 48 |}]
