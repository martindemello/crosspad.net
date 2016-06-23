open Xword

open System.IO

let qxw_main argv =
  let stream = File.OpenRead("1ac.qxw")
  let xw = Qxw.read stream
  0

let gui_main argv =
  let stream = File.OpenRead("1ac.qxw")
  let xw = Qxw.read stream
  let cursor = new Cursor(xw.cols - 1, xw.rows - 1)
  let state = { xword = xw; cursor = cursor }

  renumber xw

  GtkGui.Run(state)
  0

[<EntryPoint>]
let main argv =
  gui_main argv
