module type Day = sig
  val part1 : string list -> string
  val part2 : string list -> string
end

let read_all_lines () =
  let lines = ref [] in
  try
    while true do
      lines := read_line () :: !lines
    done
  with End_of_file -> List.rev !lines

let day_of_name =
  let open Days in
  function
  | "01" -> (module Day01 : Day)
  | "02" -> (module Day02 : Day)
  | "03" -> (module Day03 : Day)
  | "04" -> (module Day04 : Day)
  | "05" -> (module Day05 : Day)
  | "06" -> (module Day06 : Day)
  | "07" -> (module Day07 : Day)
  (* next-day *)
  | arg ->
      Printf.printf "Invalid day '%s'" arg;
      exit 1

let () =
  if Array.length Sys.argv < 3 then (
    print_endline "Usage:   ./runner.exe <day> <part>";
    print_endline "Example: ./runner.exe 01 1")
  else
    let day = Sys.argv.(1) in
    let part = Sys.argv.(2) in
    let module Day = (val day_of_name day) in
    let f =
      match part with
      | "1" -> Day.part1
      | "2" -> Day.part2
      | arg ->
          Printf.printf "Invalid part '%s'" arg;
          exit 1
    in
    let lines = read_all_lines () in
    print_endline (f lines)
