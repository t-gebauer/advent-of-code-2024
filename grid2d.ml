type t = { width : int; height : int; grid : char array array }

let create width height f =
  {
    width;
    height;
    grid = Array.init height (fun y -> Array.init width (fun x -> f (x, y)));
  }

let create_from_lines lines =
  let lines = Array.of_list lines in
  let height = Array.length lines in
  let width = String.length lines.(0) in
  create width height (fun (x, y) -> lines.(y).[x])

let get grid (x, y) = grid.grid.(y).(x)
let set grid (x, y) c = grid.grid.(y).(x) <- c

let find_all_map grid f =
  let rec iter ?(res = []) pos =
    let res = match f pos with None -> res | Some a -> a :: res in
    let x, y = pos in
    if x < grid.width - 1 then iter ~res (x + 1, y)
    else if y < grid.height - 1 then iter ~res (0, y + 1)
    else res
  in
  iter (0, 0)

let find_all grid f =
  find_all_map grid (fun pos ->
      let c = get grid pos in
      if f pos c then Some pos else None)

let find_one_opt grid f =
  let rec iter pos =
    let c = get grid pos in
    if f pos c then Some pos
    else
      let x, y = pos in
      if x < grid.width - 1 then iter (x + 1, y)
      else if y < grid.height - 1 then iter (0, y + 1)
      else None
  in
  iter (0, 0)

let find_one grid f = Option.get (find_one_opt grid f)