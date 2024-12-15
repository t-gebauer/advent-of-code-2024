let add (a, b) (c, d) = (a + c, b + d)
let sub (a, b) (c, d) = (a - c, b - d)
let mul s (x, y) = (s * x, s * y)
let div s (x, y) = (x / s, y / s)
let turn_right (x, y) = (-y, x)
let turn_left (x, y) = (y, -x)
let north = (0, -1)
let south = (0, 1)
let west = (-1, 0)
let east = (1, 0)
let up = north
let down = south
let left = west
let right = east
let north_west = (-1, -1)
let north_east = (1, -1)
let south_west = (-1, 1)
let south_east = (1, 1)
let up_left = north_west
let up_right = north_east
let down_left = south_west
let down_right = south_east

let in_rect (x, y, width, height) (a, b) =
  a >= x && b >= y && a < x + width && b < y + height

let in_rect_0 (width, height) v = in_rect (0, 0, width, height) v
