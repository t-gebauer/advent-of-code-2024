type filePart = { id : int; size : int }
type blockGroup = File of filePart | Empty of int

let partSize = function File { size; _ } -> size | Empty size -> size

let parseFileParts line =
  let parts = ref [] in
  String.iteri
    (fun i c ->
      let size = int_of_char c - int_of_char '0' in
      let p = if i mod 2 = 0 then File { id = i / 2; size } else Empty size in
      parts := p :: !parts)
    line;
  List.rev !parts

let print_part_list parts =
  List.iter
    (function
      | File { id; size } ->
          for _ = 1 to size do
            print_int id
          done
      | Empty size ->
          for _ = 1 to size do
            print_char '.'
          done)
    parts;
  print_newline ()

let%expect_test _ =
  "12345" |> parseFileParts |> print_part_list;
  [%expect {| 0..111....22222 |}]

let%expect_test _ =
  "2333133121414131402" |> parseFileParts |> print_part_list;
  [%expect {| 00...111...2...333.44.5555.6666.777.888899 |}]

let defrag1 parts =
  let files =
    parts |> List.filter_map (function File x -> Some x | _ -> None)
  in
  let totalSize = files |> List.map (fun p -> p.size) |> Lib.sum in
  let rev = List.rev files in
  let rec f res pos parts rev =
    if pos > totalSize then
      let d = pos - totalSize in
      match res with
      | File { id; size } :: rest -> File { id; size = size - d } :: rest
      | _ -> res
    else
      let part = List.hd parts in
      match part with
      | File { size; _ } -> f (part :: res) (pos + size) (List.tl parts) rev
      | Empty size ->
          let ({ size = rsize; id = rid } as x) = List.hd rev in
          if rsize = size then
            f (File x :: res) (pos + rsize) (List.tl parts) (List.tl rev)
          else if rsize < size then
            let empty = Empty (size - rsize) in
            f (File x :: res) (pos + rsize) (empty :: List.tl parts)
              (List.tl rev)
          else
            let insert = File { size; id = rid } in
            let remain = { size = rsize - size; id = rid } in
            f (insert :: res) (pos + size) (List.tl parts)
              (remain :: List.tl rev)
  in
  f [] 0 parts rev |> List.rev

let%expect_test _ =
  "12345" |> parseFileParts |> defrag1 |> print_part_list;
  [%expect {| 022111222 |}]

let%expect_test _ =
  "2333133121414131402" |> parseFileParts |> defrag1 |> print_part_list;
  [%expect {| 0099811188827773336446555566 |}]

let sum_of_n ~first ~last n = n * (first + last) / 2

let checksum parts =
  List.fold_left
    (fun (pos, sum) part ->
      match part with
      | File { id; size } ->
          let c = id * sum_of_n ~first:pos ~last:(pos + size - 1) size in
          (pos + size, sum + c)
      | Empty size -> (pos + size, sum))
    (0, 0) parts
  |> snd

let part1 lines =
  assert (List.length lines = 1);
  let line = List.hd lines in
  parseFileParts line |> defrag1 |> checksum |> string_of_int

let part2 _lines = "TODO"

let example = Lib.parse_lines {|
2333133121414131402
|}

let%expect_test _ =
  print_string (part1 example);
  [%expect {| 1928 |}]

let%expect_test _ =
  print_string (part2 example);
  [%expect {| TODO |}]
