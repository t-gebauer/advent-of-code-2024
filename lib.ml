module IntPair = struct
  type t = int * int

  let compare (a, b) (c, d) =
    match Stdlib.compare a c with 0 -> Stdlib.compare b d | c -> c
end

let parse_lines text = Str.split (Str.regexp " *\n *") text

let%expect_test _ =
  let s = {|
1 2 3

444   555
  7 8 9
|} in
  parse_lines s |> List.iter (Printf.printf "<%s>\n");
  [%expect {|
    <1 2 3>
    <>
    <444   555>
    <7 8 9>
    |}]

let regex_match_all_map regex extract string =
  let rec match_all p result =
    try
      let p' = Str.search_forward regex string p in
      let substring = Str.matched_string string in
      let res' = extract substring in
      match_all (p' + String.length substring) (res' :: result)
    with Not_found -> result
  in
  match_all 0 [] |> List.rev

let regex_match_all regexp string =
  regex_match_all_map regexp (fun a -> a) string

let string_reverse s =
  let l = String.length s in
  let b = Buffer.create l in
  let rec f i =
    Buffer.add_char b s.[i];
    if i > 0 then f (i - 1) else ()
  in
  f (l - 1);
  Buffer.contents b

let%expect_test _ =
  string_reverse "Foobar" |> print_endline;
  [%expect {| rabooF |}]

let sum = List.fold_left ( + ) 0

let number_regex = Str.regexp {|[0-9]+|}

let extract_numbers text =
  regex_match_all number_regex text |> List.map int_of_string
