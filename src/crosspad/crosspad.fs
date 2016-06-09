open Xword

open System.IO

[<EntryPoint>]
let main argv =
  let stream = File.OpenRead("test.puz")
  let xw = Puz.read stream
  let cursor = new Cursor(xw.cols - 1, xw.rows - 1)
  let state = { xword = xw; cursor = cursor }

  renumber xw

  GtkGui.Run(state)
  0
