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

let rec list_of_tree tree =
  match tree with
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

let () =
  let a = ref Empty in
  let b = ref Empty in
  try
    while true do
      let line = read_line () in
      let x, y = split_into_pairs line in
      a := tree_insert !a x;
      b := tree_insert !b y
    done
  with End_of_file ->
    Printf.printf "part 1: %d\n" (total_distance !a !b);
    Printf.printf "part 2: %d\n" (similarity_score !a !b)
