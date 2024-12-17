module V = Vec2i

let good_place_for_obstruction map pos dir =
  let map = Grid.copy map in
  Grid.set pos 'T' map;
  Grid.set (V.add pos dir) '#' map;
  let dir = ref (V.turn_right dir) in
  let pos = ref pos in
  let at_turn = ref false in
  let exception Loop in
  try
    while true do
      let p = V.add !pos !dir in
      match Grid.get p map with
      | '.' ->
          Grid.set p '+' map;
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
          Grid.set !pos 'T' map
      | _ -> failwith "invalid char on map"
    done
  with
  | Grid.Out_of_bounds -> false
  | Loop -> true

let simulate_guard map =
  let pos = ref (Grid.find_one (fun _ c -> c = '^') map) in
  let dir = ref V.north in
  try
    while true do
      Grid.set !pos 'X' map;
      let p = V.add !pos !dir in
      match Grid.get p map with
      | '#' -> dir := V.turn_right !dir
      | '.' | 'X' -> pos := p
      | _ -> failwith "invalid char on map"
    done
  with Grid.Out_of_bounds -> ()

let simulate_guard2 map =
  let pos = ref (Grid.find_one (fun _ c -> c = '^') map) in
  let dir = ref V.north in
  Grid.set !pos 'X' map;
  try
    while true do
      let p = V.add !pos !dir in
      match Grid.get p map with
      | '#' ->
          dir := V.turn_right !dir;
          Grid.set !pos (if Grid.get !pos map = 'O' then 'R' else 'T') map
      | 'O' | 'X' | 'T' | 'R' -> pos := p
      | '.' ->
          if good_place_for_obstruction map !pos !dir then Grid.set p 'O' map
          else Grid.set p 'X' map;
          pos := p
      | _ -> failwith "invalid char on map"
    done
  with Grid.Out_of_bounds -> ()

let part1 lines =
  let map = Grid.create_from_lines lines in
  simulate_guard map;
  Grid.find_all (fun _ c -> c = 'X') map |> List.length |> string_of_int

let part2 lines =
  let map = Grid.create_from_lines lines in
  simulate_guard2 map;
  Grid.find_all (fun _ c -> c = 'O' || c = 'R') map
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
