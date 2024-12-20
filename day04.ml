module V = Vec2i

let xmas = Str.regexp "XMAS"

let count_xmas line =
  let c line = Lib.regex_match_all xmas line |> List.length in
  c line + c (Lib.string_reverse line)

let string_columns_of_rows rows =
  let size = List.length rows in
  let rec f ?(res = []) i =
    try
      let b = Buffer.create size in
      List.iter (fun row -> Buffer.add_char b row.[i]) rows;
      f ~res:(Buffer.contents b :: res) (succ i)
    with Invalid_argument _ -> res
  in
  f 0 |> List.rev

let string_diagonals_of_rows rows =
  let rows = Array.of_list rows in
  let height = Array.length rows in
  let width = String.length rows.(0) in
  let rec f ?(res = []) start start_dir dir =
    let sx, sy = start in
    if sx < 0 || sy < 0 || sx >= width || sy >= height then List.rev res
    else
      let b = Buffer.create width in
      let rec f' p =
        try
          let x, y = p in
          Buffer.add_char b rows.(y).[x];
          f' (V.add p dir)
        with Invalid_argument _ -> ()
      in
      f' start;
      f ~res:(Buffer.contents b :: res) (V.add start start_dir) start_dir dir
  in
  (* from top-right to top-left : forward *)
  f (width - 1, 0) (-1, 0) (1, 1)
  (* from top-left to bottom-left : forward *)
  @ f (0, 1) (0, 1) (1, 1)
  (* from top-left to top-right : backward *)
  @ f (0, 0) (1, 0) (-1, 1)
  (* from top-right to bottom-right : backward *)
  @ f (width - 1, 1) (0, 1) (-1, 1)

let%expect_test _ =
  let rows = Lib.parse_lines {|
ABCD
EFGH
IJKL
MNOP
|} in
  let t f = f rows |> List.iter (fun row -> Printf.printf "%s\n" row) in
  t string_columns_of_rows;
  [%expect {|
    AEIM
    BFJN
    CGKO
    DHLP
    |}];
  t string_diagonals_of_rows;
  [%expect
    {|
    D
    CH
    BGL
    AFKP
    EJO
    IN
    M
    A
    BE
    CFI
    DGJM
    HKN
    LO
    P
    |}]

let part1 lines =
  let horizontal = lines in
  let vertical = string_columns_of_rows lines in
  let diagonals = string_diagonals_of_rows lines in
  [ horizontal; vertical; diagonals ]
  |> List.map (fun lines -> lines |> List.map count_xmas |> Lib.sum)
  |> Lib.sum |> string_of_int

let part2 lines =
  let open Grid in
  let grid = create_from_lines lines in
  find_all_map
    (fun p c ->
      if
        let x, y = p in
        x < 1 || y < 1 || x > grid.width - 2 || y > grid.width - 2
      then None
      else if c <> 'A' then None
      else if
        let tl = get grid (V.add p (-1, -1)) in
        let tr = get grid (V.add p (1, -1)) in
        let bl = get grid (V.add p (-1, 1)) in
        let br = get grid (V.add p (1, 1)) in
        ((tl = 'M' && br = 'S') || (tl = 'S' && br = 'M'))
        && ((tr = 'M' && bl = 'S') || (tr = 'S' && bl = 'M'))
      then Some p
      else None)
    grid
  |> List.length |> string_of_int

let example =
  Lib.parse_lines
    {|
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
|}

let%expect_test _ =
  print_string (part1 example);
  [%expect {| 18 |}]

let%expect_test _ =
  print_string (part2 example);
  [%expect {| 9 |}]
