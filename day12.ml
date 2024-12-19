module PMap = Map.Make (Lib.IntPair)
module V = Vec2i

type region = { area : int; perimiter : int }

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

type region_mut = { mutable area : int; mutable sides : int }

let find_regions2 map =
  let regions = Hashtbl.create 100 in
  let rec expand_region char pos region =
    Hashtbl.add regions pos region;
    region.area <- region.area + 1;
    [ V.up; V.down; V.left; V.right ]
    |> List.map (V.add pos)
    |> List.iter (fun p ->
           try
             if
               Grid.get p map = char
               && Option.is_none (Hashtbl.find_opt regions p)
             then expand_region char p region
           with Grid.Out_of_bounds -> ())
  in
  let count_corners char pos region =
    let get_neighbor dir =
      try Grid.get (V.add dir pos) map with Grid.Out_of_bounds -> ' '
    in
    region.sides <-
      region.sides
      + ([ V.up_left; V.up_right; V.down_left; V.down_right ]
        |> List.map (fun d ->
               let x, y = d in
               let nx = get_neighbor (x, 0) in
               let ny = get_neighbor (0, y) in
               let center = get_neighbor d in
               if
                 (nx <> char && ny <> char)
                 || (nx = char && ny = char && center <> char)
               then 1
               else 0)
        |> Lib.sum)
  in
  let reg =
    Grid.flat_mapi
      (fun pos char ->
        match Hashtbl.find_opt regions pos with
        | None ->
            let region = { area = 0; sides = 0 } in
            expand_region char pos region;
            Some region
        | Some _ -> None)
      map
  in
  Grid.iteri
    (fun pos char -> count_corners char pos (Hashtbl.find regions pos))
    map;
  reg |> List.filter_map (fun x -> x)

let bulk_diskcounted_price region = region.area * region.sides

let part2 lines =
  let map = Grid.create_from_lines lines in
  find_regions2 map
  |> List.map bulk_diskcounted_price
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
