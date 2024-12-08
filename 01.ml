type tree = Empty | Node of int * tree * tree;;

let rec tree_insert tree x =
  match tree with
  | Empty -> Node(x, Empty, Empty)
  | Node(y, left, right) ->
      if x < y then Node(y, tree_insert left x, right)
               else Node(y, left, tree_insert right x);;

let rec tree_count tree e =
  match tree with
  | Empty -> 0
  | Node(x, left, right) -> (if x = e then 1 else 0) + (tree_count left e) + (tree_count right e);;

let rec list_of_tree tree =
  match tree with
  | Empty -> []
  | Node(e, left, right) -> list_of_tree left @ e :: list_of_tree right;;

let split_in_pairs str =
  match String.split_on_char ' ' str with
  | [x; _; _; y] -> (int_of_string x, int_of_string y)
  | e -> raise (Invalid_argument "str");;

let total_distance a b =
  List.fold_left2 (fun acc x y -> let dist = abs (x - y) in acc + dist)
    0 (list_of_tree a) (list_of_tree b);;

let similarity_score a b =
  List.fold_left (fun acc e -> acc + e * (tree_count b e))
    0 (list_of_tree a);;

let a = ref Empty in
let b = ref Empty in
try
  while true do
    let line = read_line () in
    let (x, y) = split_in_pairs line in
    a := tree_insert !a x;
    b := tree_insert !b y;
  done
with End_of_file ->
  print_string "part 1: "; print_int (total_distance !a !b); print_newline ();
  print_string "part 2: "; print_int (similarity_score !a !b); print_newline ()
