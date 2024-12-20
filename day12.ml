module PMap = Map.Make (Lib.IntPair)
module PSet = Set.Make (Lib.IntPair)
module V = Vec2i

let find_regions map =
  let rec expand_region char pos region =
    let region = PSet.add pos region in
    [ V.up; V.down; V.left; V.right ]
    |> List.map (V.add pos)
    |> List.fold_left
         (fun reg p ->
           if Grid.get_opt map p = Some char && not (PSet.mem p reg) then
             expand_region char p reg
           else reg)
         region
  in
  let visited = Grid.create (Grid.size map) (fun _ -> '.') in
  Grid.flat_mapi
    (fun pos char ->
      match Grid.get visited pos with
      | '.' ->
          let region = expand_region char pos PSet.empty in
          PSet.iter (fun p -> Grid.set visited p '+') region;
          Some region
      | _ -> None)
    map
  |> List.filter_map (fun x -> x)

let count_perimiter region =
  let count_borders pos =
    let has_neighbor dir = PSet.mem (V.add pos dir) region in
    [ V.up; V.down; V.left; V.right ]
    |> List.map (fun p -> if has_neighbor p then 0 else 1)
    |> Lib.sum
  in
  PSet.to_list region |> List.map count_borders |> Lib.sum

let part1 lines =
  let map = Grid.create_from_lines lines in
  find_regions map
  |> List.map (fun reg ->
         let area = PSet.cardinal reg in
         let perimiter = count_perimiter reg in
         area * perimiter)
  |> Lib.sum |> string_of_int

let count_sides region =
  let count_corners pos =
    let has_neighbor dir = PSet.mem (V.add pos dir) region in
    [ V.up_left; V.up_right; V.down_left; V.down_right ]
    |> List.map (fun d ->
           let x, y = d in
           let nx = has_neighbor (x, 0) in
           let ny = has_neighbor (0, y) in
           let center = has_neighbor d in
           if ((not nx) && not ny) || (nx && ny && not center) then 1 else 0)
    |> Lib.sum
  in
  PSet.to_list region |> List.map count_corners |> Lib.sum

let part2 lines =
  let map = Grid.create_from_lines lines in
  find_regions map
  |> List.map (fun reg ->
         let area = PSet.cardinal reg in
         let sides = count_sides reg in
         area * sides)
  |> Lib.sum |> string_of_int

let example1 = Lib.parse_lines {|
AAAA
BBCD
BBCC
EEEC
|}

let%expect_test _ =
  print_string (part1 example1);
  [%expect {| 140 |}]

let%expect_test _ =
  print_string (part2 example1);
  [%expect {| 80 |}]

let example2 = Lib.parse_lines {|
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
|}

let%expect_test _ =
  print_string (part1 example2);
  [%expect {| 772 |}]

let%expect_test _ =
  print_string (part2 example2);
  [%expect {| 436 |}]

let example3 =
  Lib.parse_lines
    {|
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
|}

let%expect_test _ =
  print_string (part1 example3);
  [%expect {| 1930 |}]

let%expect_test _ =
  print_string (part2 example3);
  [%expect {| 1206 |}]

let example4 = Lib.parse_lines {|
EEEEE
EXXXX
EEEEE
EXXXX
EEEEE
|}

let%expect_test _ =
  print_string (part2 example4);
  [%expect {| 236 |}]

let example5 = Lib.parse_lines {|
AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA
|}

let%expect_test _ =
  print_string (part2 example5);
  [%expect {| 368 |}]
