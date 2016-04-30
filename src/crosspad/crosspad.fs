module crosspad

open Eto.Forms
open Eto.Drawing

[<EntryPoint>]
let main argv =
  let app = new Application()
  let d = new Drawable()

  let rows = 15
  let cols = 15

  let grid = Array2D.create rows cols ""

  grid.[1, 2] <- "#"

  let blackBrush = new SolidBrush(Colors.Black)
  let whiteBrush = new SolidBrush(Colors.White)

  d.Paint.Add(fun e ->
    let scale = 30.f
    let g = e.Graphics
    g.TranslateTransform(100.f, 100.f)

    for row = 0 to rows do
      let pos = single(row) * scale
      let max = single(cols) * scale
      g.DrawLine(Colors.Black, 0.f, pos, max, pos)

    for col = 0 to cols do
      let pos = single(col) * scale
      let max = single(rows) * scale
      g.DrawLine(Colors.Black, pos, 0.f, pos, max)

    for row = 0 to rows - 1 do
      for col = 0 to cols - 1 do
        let x = single(col) * scale;
        let y = single(row) * scale;
        let brush =
          if System.String.Equals(grid.[row, col], "#") then
            blackBrush
          else
            whiteBrush
        g.FillRectangle(brush, x + 1.f, y + 1.f, scale - 1.f, scale - 1.f)
  )




  let f = new Form(Topmost=true, ClientSize = new Size(600, 480))
  f.Content <- d
  f.Show()
  app.Run()
  0
