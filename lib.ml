let parse_lines text = String.split_on_char '\n' text |> List.filter (( <> ) "")

