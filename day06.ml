let vec_add (a, b) (c, d) = (a + c, b + d)

let rotate_right = function
  | 1, 0 -> (0, 1)
  | 0, 1 -> (-1, 0)
  | -1, 0 -> (0, -1)
  | 0, -1 -> (1, 0)
  | _ -> failwith "invalid direction vector"

let part1 lines =
  let map = Grid2d.create_from_lines lines in
  let pos = ref (Grid2d.find_one map (fun _ c -> c = '^')) in
  let dir = ref (0, -1) in
  try
    while true do
      Grid2d.set map !pos 'X';
      let p = vec_add !pos !dir in
      match Grid2d.get map p with
      | '#' -> dir := rotate_right !dir
      | _ -> pos := p
    done
  with Invalid_argument _ ->
    Grid2d.find_all map (fun _ c -> c = 'X')
    |> List.fold_left (fun a _ -> a + 1) 0
    |> string_of_int

let part2 _lines = "TODO"

let example =
  Lib.parse_lines
    {|
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
|}

let%expect_test _ =
  print_string (part1 example);
  [%expect {| 41 |}]

let%expect_test _ =
  print_string (part2 example);
  [%expect {| TODO |}]
