type region = { area : int; perimiter : int }

module V = Vec2i

let find_regions map =
  let visited = Grid.(create map.width map.height (fun _ -> '.')) in
  let rec expand_region id pos : region =
    Grid.set pos '+' visited;
    [ V.up; V.down; V.left; V.right ]
    |> List.map (V.add pos)
    |> List.map (fun p ->
           let c = try Grid.get p map with Grid.Out_of_bounds -> ' ' in
           if c <> id then { area = 0; perimiter = 1 }
           else
             let vis = Grid.get p visited in
             if vis = '+' then { area = 0; perimiter = 0 }
             else expand_region id p)
    |> List.fold_left
         (fun acc reg ->
           {
             area = acc.area + reg.area;
             perimiter = acc.perimiter + reg.perimiter;
           })
         { area = 1; perimiter = 0 }
  in
  Grid.flat_mapi
    (fun p c ->
      if not (Grid.get p visited = '+') then Some (expand_region c p) else None)
    map
  |> List.filter_map (fun x -> x)

let price_of_region region = region.area * region.perimiter

let part1 lines =
  let map = Grid.create_from_lines lines in
  find_regions map |> List.map price_of_region |> Lib.sum |> string_of_int

let part2 _lines = "TODO"

let example1 = Lib.parse_lines {|
AAAA
BBCD
BBCC
EEEC
|}

let%expect_test _ =
  print_string (part1 example1);
  [%expect {| 140 |}]

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
  print_string (part2 example1);
  [%expect {| TODO |}]
