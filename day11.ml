let is_even n = n mod 2 = 0
let number_of_digits num = string_of_int num |> String.length

let split_stone stone =
  let s = string_of_int stone in
  let half = String.length s / 2 in
  [ String.sub s 0 half; String.sub s half half ] |> List.map int_of_string

let blink stones =
  stones
  |> List.map (fun stone ->
         if stone = 0 then [ 1 ]
         else if is_even (number_of_digits stone) then split_stone stone
         else [ stone * 2024 ])
  |> List.flatten

let blink_multiple amount stones =
  let rec f times stones =
    if times = 0 then stones else f (times - 1) (blink stones)
  in
  f amount stones

let example_stones = Lib.extract_numbers "0 1 10 99 999"

let%expect_test _ =
  blink_multiple 1 example_stones |> List.iter (fun s -> Printf.printf "%i " s);
  [%expect {| 1 2024 1 0 9 9 2021976 |}]

let example_stones2 = Lib.extract_numbers "125 17"

let%expect_test _ =
  blink_multiple 6 example_stones2 |> List.length |> print_int;
  [%expect {| 22 |}]

let blink_count_sum times stones =
  let h = Hashtbl.create 10000 in
  let rec blink_count times stone =
    try Hashtbl.find h (times, stone)
    with Not_found ->
      let c =
        if times = 0 then 1
        else if stone = 0 then blink_count (times - 1) 1
        else if is_even (number_of_digits stone) then
          split_stone stone |> List.map (blink_count (times - 1)) |> Lib.sum
        else blink_count (times - 1) (stone * 2024)
      in
      Hashtbl.add h (times, stone) c;
      c
  in
  List.map (blink_count times) stones |> Lib.sum

let%expect_test _ =
  blink_count_sum 6 example_stones2 |> print_int;
  [%expect {| 22 |}]

let%expect_test _ =
  blink_count_sum 25 example_stones2 |> print_int;
  [%expect {| 55312 |}]

let part1 lines =
  let stones = Lib.extract_numbers (List.hd lines) in
  blink_multiple 25 stones |> List.length |> string_of_int

let part2 lines =
  let stones = Lib.extract_numbers (List.hd lines) in
  blink_count_sum 75 stones |> string_of_int

let example = [ "125 17" ]

let%expect_test _ =
  print_string (part1 example);
  [%expect {| 55312 |}]

let%expect_test _ =
  print_string (part2 example);
  [%expect {| 65601038650482 |}]
