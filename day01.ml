type tree = Empty | Node of int * tree * tree

let rec tree_insert tree x =
  match tree with
  | Empty -> Node (x, Empty, Empty)
  | Node (n, left, right) ->
      if x < n then Node (n, tree_insert left x, right)
      else Node (n, left, tree_insert right x)

let rec tree_count tree x =
  match tree with
  | Empty -> 0
  | Node (n, left, right) ->
      if x < n then 0 + tree_count left x
      else (if x = n then 1 else 0) + tree_count right x

let rec list_of_tree = function
  | Empty -> []
  | Node (e, left, right) -> list_of_tree left @ (e :: list_of_tree right)

let split_into_pairs str =
  match String.split_on_char ' ' str with
  | [ x; _; _; y ] -> (int_of_string x, int_of_string y)
  | _ -> raise (Invalid_argument "str")

let total_distance a b =
  List.fold_left2
    (fun acc x y ->
      let dist = abs (x - y) in
      acc + dist)
    0 (list_of_tree a) (list_of_tree b)

let similarity_score a b =
  List.fold_left (fun acc e -> acc + (e * tree_count b e)) 0 (list_of_tree a)

let input_as_trees lines =
  List.fold_left
    (fun (a, b) line ->
      let x, y = split_into_pairs line in
      (tree_insert a x, tree_insert b y))
    (Empty, Empty) lines

let part1 lines =
  let a, b = input_as_trees lines in
  total_distance a b

let part2 lines =
  let a, b = input_as_trees lines in
  similarity_score a b

let parse_lines text = String.split_on_char '\n' text |> List.filter (( <> ) "")

let example = parse_lines {|
3   4
4   3
2   5
1   3
3   9
3   3
|}

let%expect_test _ =
  print_int (part1 example);
  [%expect {| 11 |}]

let%expect_test _ =
  print_int (part2 example);
  [%expect {| 31 |}]

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
