module PMap = Map.Make (Lib.IntPair)
module PSet = Set.Make (Lib.IntPair)
module V = Vec2i

let find_trails map =
  let trail_ends =
    Grid.find_all (fun _ c -> c = '9') map |> List.map (fun p -> (p, p))
  in
  let next_char char = char |> int_of_char |> pred |> char_of_int in
  let rec f positions char =
    let next_p =
      List.map
        (fun (spos, cpos) ->
          [ V.up; V.down; V.left; V.right ]
          |> List.map (fun v -> (spos, V.add cpos v))
          |> List.filter (fun (_, p) ->
                 try Grid.get p map = char with Grid.Out_of_bounds -> false))
        positions
      |> List.flatten
    in
    if char = '0' then next_p else f next_p (next_char char)
  in
  f trail_ends '8'

let distinct_ending_positions trails =
  trails
  |> List.fold_left
       (fun acc (start, pos) ->
         PMap.update pos
           (function
             | None -> Some (PSet.singleton start)
             | Some set -> Some (PSet.add start set))
           acc)
       PMap.empty
  |> PMap.map (fun s -> PSet.to_list s |> List.length)
  |> PMap.to_list |> List.map snd

(* trailhead score: how many distinct trail endings are reachable *)
let part1 lines =
  let map = Grid.create_from_lines lines in
  find_trails map |> distinct_ending_positions |> Lib.sum |> string_of_int

(* trailhead rating: how many distinct trails begin here *)
let part2 lines =
  let map = Grid.create_from_lines lines in
  find_trails map |> List.length |> string_of_int

let example =
  Lib.parse_lines
    {|
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
|}

let%expect_test _ =
  print_string (part1 example);
  [%expect {| 36 |}]

let%expect_test _ =
  print_string (part2 example);
  [%expect {| 81 |}]
