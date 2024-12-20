type t = { width : int; height : int; grid : char array array }

exception Out_of_bounds

let create (width, height) f =
  {
    width;
    height;
    grid = Array.init height (fun y -> Array.init width (fun x -> f (x, y)));
  }

let create_from_lines lines =
  let lines = Array.of_list lines in
  let height = Array.length lines in
  let width = String.length lines.(0) in
  create (width, height) (fun (x, y) -> lines.(y).[x])

let size grid = (grid.width, grid.height)

let copy grid =
  {
    width = grid.width;
    height = grid.height;
    grid = Array.init grid.height (fun y -> Array.copy grid.grid.(y));
  }

let get grid (x, y) =
  try grid.grid.(y).(x) with Invalid_argument _ -> raise Out_of_bounds

let get_opt grid pos = try Some (get grid pos) with Out_of_bounds -> None

let set grid (x, y) c =
  try grid.grid.(y).(x) <- c with Invalid_argument _ -> raise Out_of_bounds

let set_if_inside grid p c = try set grid p c with Out_of_bounds -> ()

let find_all_map f grid =
  let rec iter ?(res = []) pos =
    let c = get grid pos in
    let res = match f pos c with None -> res | Some a -> a :: res in
    let x, y = pos in
    if x < grid.width - 1 then iter ~res (x + 1, y)
    else if y < grid.height - 1 then iter ~res (0, y + 1)
    else res
  in
  iter (0, 0)

let find_all f grid =
  find_all_map (fun pos c -> if f pos c then Some pos else None) grid

let find_one_opt f grid =
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

let find_one f grid = Option.get (find_one_opt f grid)

(* let iteri f grid = *)
(*   Array.iteri (fun y line -> Array.iteri (fun x c -> f (x, y) c) line) grid.grid *)

let flat_mapi f grid =
  Array.mapi
    (fun y line -> Array.mapi (fun x c -> f (x, y) c) line |> Array.to_list)
    grid.grid
  |> Array.to_list |> List.flatten

let print grid =
  Array.iter
    (fun line ->
      Array.iter
        (fun c ->
          print_char c;
          print_char ' ')
        line;
      print_newline ())
    grid.grid
