type report = int list
type change = Unknown | Increasing | Decreasing

let parse_report line : report =
  String.split_on_char ' ' line |> List.map int_of_string

let report_is_safe ?(dampener = false) report : bool =
  let rec is_safe dampener change prev remaining : bool =
    (dampener && is_safe false change prev (List.tl remaining))
    ||
    match remaining with
    | [] -> true
    | current :: remaining -> (
        match prev with
        | None -> is_safe dampener Unknown (Some current) remaining
        | Some prev ->
            let diff = current - prev in
            diff <> 0
            && abs diff >= 1
            && abs diff <= 3
            &&
            let new_change = if diff > 0 then Increasing else Decreasing in
            (change = Unknown || new_change = change)
            && is_safe dampener new_change (Some current) remaining)
  in
  is_safe dampener Unknown None report

let%expect_test "report_is_safe" =
  let t r = r |> parse_report |> report_is_safe |> Printf.printf "%b\n" in
  t "7 6 4 2 1";
  [%expect {| true |}];
  t "1 2 7 8 9";
  [%expect {| false |}];
  t "9 7 6 2 1";
  [%expect {| false |}];
  t "1 3 2 4 5";
  [%expect {| false |}];
  t "8 6 4 4 1";
  [%expect {| false |}];
  t "1 3 6 7 9";
  [%expect {| true |}]

let part1 lines =
  List.map parse_report lines
  |> List.fold_left
       (fun acc report -> acc + if report_is_safe report then 1 else 0)
       0

let%expect_test "report_is_safe with dampener" =
  let t r =
    r |> parse_report |> report_is_safe ~dampener:true |> Printf.printf "%b\n"
  in
  t "7 6 4 2 1";
  [%expect {| true |}];
  t "1 2 7 8 9";
  [%expect {| false |}];
  t "9 7 6 2 1";
  [%expect {| false |}];
  t "1 3 2 4 5";
  [%expect {| true |}];
  t "8 6 4 4 1";
  [%expect {| true |}];
  t "1 3 6 7 9";
  [%expect {| true |}]

let part2 lines =
  List.map parse_report lines
  |> List.fold_left
       (fun acc report ->
         acc + if report_is_safe ~dampener:true report then 1 else 0)
       0

let parse_lines text = String.split_on_char '\n' text |> List.filter (( <> ) "")

let example =
  parse_lines {|
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
|}

let%expect_test "part1" =
  print_int (part1 example);
  [%expect {| 2 |}]

let read_all_lines () =
  let lines = ref [] in
  try
    while true do
      lines := read_line () :: !lines
    done
  with End_of_file -> List.rev !lines

let main () =
  let lines = read_all_lines () in
  Printf.printf "part 1: %d\n" (part1 lines);
  Printf.printf "part 2: %d\n" (part2 lines)
