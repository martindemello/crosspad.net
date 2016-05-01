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

let setLetter (xword : xword) (cursor : Cursor) (letter : string) =
  let row, col = cursor.Y, cursor.X
  xword.grid.[row, col] <- 
    { xword.grid.[row, col] with cell = Letter letter }
  
let toggleBlack (xword : xword) (cursor : Cursor) =
  let row, col = cursor.Y, cursor.X
  let cell = xword.grid.[row, col].cell
  xword.grid.[row, col] <- 
    match cell with
    | Black -> { xword.grid.[row, col] with cell = Empty }
    | Empty -> { xword.grid.[row, col] with cell = Black }
    | _ -> xword.grid.[row, col]
