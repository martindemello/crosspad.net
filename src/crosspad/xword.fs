module Xword

type cell = Black | Empty | Letter of string

type direction = Across | Down

type square = {
  cell : cell
  num : int
}

type light = {
  x : int
  y : int
  dir : direction
  num : int
  word : string
}

type clue = {
  mutable light : light
  mutable clue : string
  mutable edited_clue : string
}

let empty_clue = {
  light = { x = 1; y = 1; dir = Across; num = 1; word = "" }
  clue = ""
  edited_clue = ""
}

type clues = {
  across : ResizeArray<clue>
  down : ResizeArray<clue>
}

type xword = {
  rows : int
  cols : int
  grid : square [,]
  clues : clues
}

let letter_of_cell = function
  | Black -> "#"
  | Empty -> "."
  | Letter c -> c

let string_of_dir = function
  | Across -> "A" | Down -> "D"

let string_of_clue clue =
  sprintf "%d%s. %s" clue.light.num (string_of_dir clue.light.dir) clue.clue

let make_xword(rows, cols) =
  let grid = Array2D.create rows cols { cell = Empty; num = 0 }
  let clues = { across = new ResizeArray<clue> (); down = new ResizeArray<clue> () }
  { rows = rows; cols = cols; grid = grid; clues = clues }

type cursor_movement =
  | Move_Left
  | Move_Right
  | Move_Up
  | Move_Down
  | Move_Across

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

let readWord xw x y dx dy =
  let rec read x y acc =
    if boundary xw x y then
      String.concat "" (List.rev acc)
    else
      let c = letter_of_cell xw.grid.[y, x].cell
      read (x + dx) (y + dy) (c :: acc)
  read x y []

let wordAcross xw x y =
  match startAcross xw x y with
  | true -> Some (readWord xw x y 1 0)
  | false -> None

let wordDown xw x y =
  match startDown xw x y with
  | true -> Some (readWord xw x y 0 1)
  | false -> None

let setNum xw x y n =
  xw.grid.[y, x] <-
    { xw.grid.[y, x] with num = n }

let renumberWithCallbacks on_ac on_dn xw =
  let mutable n = 1 in
  for y = 0 to xw.rows - 1 do
    for x = 0 to xw.cols - 1 do
      let a, d = wordAcross xw x y, wordDown xw x y
      Option.iter (fun w -> on_ac {x = x; y = y; num = n; word = w; dir = Across}) a
      Option.iter (fun w -> on_dn {x = x; y = y; num = n; word = w; dir = Down}) d
      if (a.IsSome || d.IsSome) then begin
        setNum xw x y n
        n <- n + 1
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

