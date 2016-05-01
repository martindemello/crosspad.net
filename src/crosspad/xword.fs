module Xword

type cell = Black | Empty | Letter of string

type square = {
  cell : cell
  num : int
}

type xword = {
  rows : int;
  cols : int;
  grid : square [,];
}

type cursor_movement = 
  | Move_Left
  | Move_Right
  | Move_Up
  | Move_Down
  | Move_Across
  | Move_Bksp_Ac
  | Move_Bksp_Dn

type Cursor(xmax: int, ymax: int) =
  let xmax = xmax
  let ymax = ymax
  let mutable x = 0
  let mutable y = 0
  
  let checkBounds(n, max) =
    if n > max || n < 0 then
      raise (new System.Exception("Cursor out of bounds"))
    n

  let pin(n, max) =
    if n < 0 then 0 else if n >= max then max - 1 else x

  let cycle(n, max) =
    (n + max) % max

  let delta = function
    | Move_Left -> (-1, 0)
    | Move_Right -> (1, 0)
    | Move_Up -> (0, -1)
    | Move_Down -> (0, 1)
    | Move_Across -> (1, 0)
    | Move_Bksp_Ac -> (-1, 0)
    | Move_Bksp_Dn -> (0, -1)

  member this.X
    with get() = x
    and set(x') = x <- checkBounds(x', xmax)
  
  member this.Y
    with get() = y
    and set(y') = y <- checkBounds(y', ymax)

  member this.Move(dir : cursor_movement, ?wrap : bool) =
    let wrap = defaultArg wrap true
    let dx, dy = delta dir
    let fn = if wrap then cycle else pin
    this.X <- fn(this.X + dx, xmax)
    this.Y <- fn(this.Y + dy, ymax)

type state = {
  xword : xword
  cursor : Cursor
}

// xword manipulation

let setLetter (xw : xword) (cursor : Cursor) (letter : string) =
  let row, col = cursor.Y, cursor.X
  xw.grid.[row, col] <- 
    { xw.grid.[row, col] with cell = Letter letter }
  
let toggleBlack (xw : xword) (cursor : Cursor) =
  let row, col = cursor.Y, cursor.X
  let cell = xw.grid.[row, col].cell
  xw.grid.[row, col] <- 
    match cell with
    | Black -> { xw.grid.[row, col] with cell = Empty }
    | Empty -> { xw.grid.[row, col] with cell = Black }
    | _ -> xw.grid.[row, col]

// numbering

let isBlack xw x y = xw.grid.[y, x].cell = Black

let boundary xw x y =
  (x < 0) || (y < 0) ||
  (x >= xw.cols) || (y >= xw.rows) ||
  isBlack xw x y

let nonBoundary xw x y = not (boundary xw x y)

let startAcross xw x y =
  (boundary xw (x - 1) y) &&
  (nonBoundary xw x y) &&
  (nonBoundary xw (x + 1) y)

let startDown xw x y =
  (boundary xw x (y - 1)) &&
  (nonBoundary xw x y) &&
  (nonBoundary xw x (y + 1))

let setNum xw x y n =
  xw.grid.[y, x] <- 
    { xw.grid.[y, x] with num = n }

let renumberWithCallbacks on_ac on_dn xw =
  let n = ref 1 in
  for y = 0 to xw.rows - 1 do
    for x = 0 to xw.cols - 1 do
      let a, d = startAcross xw x y, startDown xw x y in
      if a then on_ac !n
      if d then on_dn !n
      if (a || d) then begin
        setNum xw x y !n
        n := !n + 1
      end
      else
        setNum xw x y 0
    done
  done

let renumber xw =
  renumberWithCallbacks ignore ignore xw
