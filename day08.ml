module CharMap = Map.Make (Char)
module V = Vec2i

let find_antennas map =
  Grid.find_all_map map (fun p c ->
      match c with '.' -> None | c -> Some (c, p))
  |> List.fold_left (fun m (c, p) -> CharMap.add_to_list c p m) CharMap.empty

let naive_calculation _width _height a b =
  [ V.add a (V.sub a b); V.add b (V.sub b a) ]

let resonant_harmonics_calculation width height a b =
  let in_bounds = V.in_rect_0 (width, height) in
  let d = V.sub b a in
  let rec f mult res =
    let p = V.add a (V.mul mult d) in
    if not (in_bounds p) then res
    else
      let next = if mult < 0 then mult - 1 else mult + 1 in
      f next (p :: res)
  in
  [ a ] |> f 1 |> f (-1)

let calculate_antinodes calc_f map antennas =
  let antinodes = Grid.(create map.width map.height (fun _ -> '.')) in
  CharMap.iter
    (fun _char positions ->
      let rec f = function
        | [] -> ()
        | a :: rest ->
            List.iter
              (fun b ->
                List.iter
                  (fun p -> Grid.set_if_inside antinodes p '#')
                  (calc_f antinodes.width antinodes.height a b))
              rest;
            f rest
      in
      f positions)
    antennas;
  antinodes

let count_antinodes calc_f lines =
  let map = Grid.create_from_lines lines in
  let antennas = find_antennas map in
  let antinodes = calculate_antinodes calc_f map antennas in
  Grid.find_all antinodes (fun _ c -> c == '#') |> List.length |> string_of_int

let part1 lines = count_antinodes naive_calculation lines
let part2 lines = count_antinodes resonant_harmonics_calculation lines

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
  [%expect {| 34 |}]
