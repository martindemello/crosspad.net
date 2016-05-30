open Xword

[<EntryPoint>]
let main argv =
  let rows = 15
  let cols = 15

  let grid = Array2D.create rows cols { cell = Empty; num = 0 }
  let clues = { across = []; down = [] }
  let xw = { rows = rows; cols = cols; grid = grid; clues = clues }
  let cursor = new Cursor(xw.cols - 1, xw.rows - 1)
  let state = { xword = xw; cursor = cursor }

  grid.[1, 2] <- { cell = Black; num = 1 }
  grid.[1, 1] <- { cell = Letter "H"; num = 2 }
  grid.[2, 1] <- { cell = Letter "E"; num = 3 }
  renumber xw

  GtkGui.Run(state)
  0
