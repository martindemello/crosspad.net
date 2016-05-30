module Xword

type cell = Black | Empty | Letter of string

type square = {
  cell : cell
  num : int
}

type clue = {
  answer : string
  clue : string
}

type clues = {
  across : clue list
  down : clue list
}

type xword = {
  rows : int
  cols : int
  grid : square [,]
  clues : clues
}

type cursor_movement = 
  | Move_Left
  | Move_Right
  | Move_Up
  | Move_Down
  | Move_Across

type cursor_direction = Across | Down

type Cursor(xmax: int, ymax: int) =
  let xmax = xmax
  let ymax = ymax
  let mutable x = 0
  let mutable y = 0
  let mutable dir = Across
  
  let checkBounds(n, max) =
    if n > max || n < 0 then
      raise (new System.Exception("Cursor out of bounds"))
    n

  let pin(n, max) =
    if n < 0 then 0 else if n > max then max else n

  let cycle(n, max) =
    (n + max + 1) % (max + 1)

  let delta = function
    | Move_Left -> (-1, 0)
    | Move_Right -> (1, 0)
    | Move_Up -> (0, -1)
    | Move_Down -> (0, 1)
    | Move_Across -> (1, 0)

  member this.X
    with get() = x
    and set(x') = x <- checkBounds(x', xmax)
  
  member this.Y
    with get() = y
    and set(y') = y <- checkBounds(y', ymax)

  member this.Dir
    with get() = dir
    and set(dir') = dir <- dir'

  member this.InBounds(x, y) =
    x >= 0 && x <= xmax && y >= 0 && y <= ymax

  member this.FlipDir() =
    this.Dir <- match this.Dir with
                | Across -> Down
                | Down -> Across

  member this.Symmetric() =
    xmax - this.X, ymax - this.Y

  member this.IsSymmetric(x, y) =
    (x, y) = this.Symmetric()

  member this.Move(dir : cursor_movement, ?wrap : bool) =
    let wrap = defaultArg wrap true
    let dx, dy = delta dir
    let fn = if wrap then cycle else pin
    this.X <- fn(this.X + dx, xmax)
    this.Y <- fn(this.Y + dy, ymax)

  member this.Advance() =
    let d = match this.Dir with
            | Across -> Move_Right
            | Down -> Move_Down
    this.Move(d, false)

  member this.Backspace() =
    let d = match this.Dir with
            | Across -> Move_Left
            | Down -> Move_Up
    this.Move(d, false)

  member this.MoveTo(x, y) =
    if this.InBounds(x, y) then
      this.X <- x
      this.Y <- y

type state = {
  xword : xword
  cursor : Cursor
}

// numbering
// this section uses (x, y) coordinates for convenience.

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

// xword manipulation

let isLetter xw x y =
  match xw.grid.[y, x].cell with
  | Letter _ -> true
  | _ -> false

let setCell xw x y c =
  xw.grid.[y, x] <- { xw.grid.[y, x] with cell = c }

// high-level functions called by the front end

let setLetter (xw : xword) (cursor : Cursor) (letter : string) =
  let row, col = cursor.Y, cursor.X
  xw.grid.[row, col] <- 
    { xw.grid.[row, col] with cell = Letter letter }
  cursor.Advance()
  
let deleteLetter (xw : xword) (cursor : Cursor) (bksp : bool) =
  if bksp then cursor.Backspace()
  let row, col = cursor.Y, cursor.X
  if isLetter xw col row then
    setCell xw col row Empty

let toggleBlack (xw : xword) (cursor : Cursor) =
  let col, row = cursor.X, cursor.Y
  let col', row' = cursor.Symmetric()
  let cell = xw.grid.[row, col].cell
  if not (isLetter xw col row || isLetter xw col' row') then
    let cell' = match cell with Black -> Empty | _ -> Black
    setCell xw col row cell'
    setCell xw col' row' cell'
    cursor.Advance()
    renumber xw

