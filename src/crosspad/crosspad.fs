open Xword

[<EntryPoint>]
let main argv =
  let rows = 15
  let cols = 15

  let xw = make_xword(rows, cols)
  let cursor = new Cursor(xw.cols - 1, xw.rows - 1)
  let state = { xword = xw; cursor = cursor }

  xw.clues.across.Add({answer = "foo"; clue = "bar"; edited_clue = ""})
  xw.clues.across.Add({answer = "baz"; clue = "quux"; edited_clue = ""})

  renumber xw

  GtkGui.Run(state)
  0
