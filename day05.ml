let list_split_at pred list =
  let p1, p2 =
    let beforeSep = ref true in
    List.partition
      (fun item ->
        if pred item then beforeSep := false;
        !beforeSep)
      list
  in
  (* discard empty line *)
  let p2 = List.tl p2 in
  (p1, p2)

let parse_input lines =
  let rule_lines, update_lines = list_split_at (( = ) "") lines in
  let rules =
    List.map Lib.extract_numbers rule_lines
    |> List.map (fun nums -> (List.nth nums 0, List.nth nums 1))
  in
  let updates = List.map Lib.extract_numbers update_lines in
  (rules, updates)

let pair_correctly_ordered rules a b =
  List.for_all (fun (l, h) -> not (a == h && b == l)) rules

let all_correctly_ordered rules pages =
  let rec f = function
    | [] -> true
    | page :: rest ->
        List.for_all (pair_correctly_ordered rules page) rest && f rest
  in
  f pages

let sum_middle_page_numbers updates =
  updates
  |> List.map (fun update ->
         let middle_page = List.nth update (List.length update / 2) in
         middle_page)
  |> List.fold_left ( + ) 0 |> string_of_int

let part1 lines =
  let rules, updates = parse_input lines in
  updates
  |> List.filter (all_correctly_ordered rules)
  |> sum_middle_page_numbers

let order_correctly rules pages =
  let rec sort = function
    | [] -> []
    | x :: xs ->
        let before, after = List.partition (fun y -> pair_correctly_ordered rules y x) xs in
        (sort before) @ x :: (sort after)
  in sort pages

let part2 lines =
  let rules, updates = parse_input lines in
  updates
  |> List.filter (fun pages -> not (all_correctly_ordered rules pages))
  |> List.map (order_correctly rules)
  |> sum_middle_page_numbers

let example =
  Lib.parse_lines
    {|
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
|}

let%expect_test _ =
  print_string (part1 example);
  [%expect {| 143 |}]

let%expect_test _ =
  print_string (part2 example);
  [%expect {| 123 |}]
