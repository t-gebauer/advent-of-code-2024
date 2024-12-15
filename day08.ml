module CharMap = Map.Make (Char)
module V = Vec2i

let find_antennas map =
  let a =
    Grid.find_all_map map (fun p c ->
        match c with '.' -> None | c -> Some (c, p))
  in
  List.fold_left (fun m (c, p) -> CharMap.add_to_list c p m) CharMap.empty a

let calculate_antinodes map antennas =
  let antinodes = Grid.(create map.width map.height (fun _ -> '.')) in
  CharMap.iter
    (fun _char positions ->
      let rec f = function
        | [] -> ()
        | a :: rest ->
            List.iter
              (fun b ->
                Grid.set_if_inside antinodes (V.add a (V.sub a b)) '#';
                Grid.set_if_inside antinodes (V.add b (V.sub b a)) '#')
              rest;
            f rest
      in
      f positions)
    antennas;
  antinodes

let part1 lines =
  let map = Grid.create_from_lines lines in
  let antennas = find_antennas map in
  let antinodes = calculate_antinodes map antennas in
  Grid.find_all antinodes (fun _ c -> c == '#') |> List.length |> string_of_int

let part2 _lines = "TODO"

let example =
  Lib.parse_lines
    {|
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
|}

let%expect_test _ =
  print_string (part1 example);
  [%expect {| 14 |}]

let%expect_test _ =
  print_string (part2 example);
  [%expect {| TODO |}]
