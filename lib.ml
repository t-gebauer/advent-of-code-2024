let parse_lines text = String.split_on_char '\n' text |> List.filter (( <> ) "")

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
