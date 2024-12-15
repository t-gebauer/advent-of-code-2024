module V = Vec2i

let good_place_for_obstruction map pos dir =
  let map = Grid.copy map in
  Grid.set map pos 'T';
  Grid.set map (V.add pos dir) '#';
  let dir = ref (V.turn_right dir) in
  let pos = ref pos in
  let at_turn = ref false in
  let exception Loop in
  try
    while true do
      let p = V.add !pos !dir in
      match Grid.get map p with
      | '.' ->
          Grid.set map p '+';
          at_turn := false;
          pos := p
      | 'X' | 'O' | '+' ->
          at_turn := false;
          pos := p
      | 'T' | 'R' ->
          at_turn := true;
          pos := p
      | '#' ->
          if !at_turn then raise Loop else dir := V.turn_right !dir;
          Grid.set map !pos 'T'
      | _ -> failwith "invalid char on map"
    done
  with
  | Grid.Out_of_bounds -> false
  | Loop -> true

let simulate_guard map =
  let pos = ref (Grid.find_one map (fun _ c -> c = '^')) in
  let dir = ref V.north in
  try
    while true do
      Grid.set map !pos 'X';
      let p = V.add !pos !dir in
      match Grid.get map p with
      | '#' -> dir := V.turn_right !dir
      | '.' | 'X' -> pos := p
      | _ -> failwith "invalid char on map"
    done
  with Grid.Out_of_bounds -> ()

let simulate_guard2 map =
  let pos = ref (Grid.find_one map (fun _ c -> c = '^')) in
  let dir = ref V.north in
  Grid.set map !pos 'X';
  try
    while true do
      let p = V.add !pos !dir in
      match Grid.get map p with
      | '#' ->
          dir := V.turn_right !dir;
          Grid.set map !pos (if Grid.get map !pos = 'O' then 'R' else 'T')
      | 'O' | 'X' | 'T' | 'R' -> pos := p
      | '.' ->
          if good_place_for_obstruction map !pos !dir then Grid.set map p 'O'
          else Grid.set map p 'X';
          pos := p
      | _ -> failwith "invalid char on map"
    done
  with Grid.Out_of_bounds -> ()

let part1 lines =
  let map = Grid.create_from_lines lines in
  simulate_guard map;
  Grid.find_all map (fun _ c -> c = 'X') |> List.length |> string_of_int

let part2 lines =
  let map = Grid.create_from_lines lines in
  simulate_guard2 map;
  Grid.find_all map (fun _ c -> c = 'O' || c = 'R')
  |> List.length |> string_of_int

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
  [%expect {| 6 |}]
