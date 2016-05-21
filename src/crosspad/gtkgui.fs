module GtkGui

open System
open Gtk
open Cairo
open Xword

let (width, height) = (800, 600)

let scale = 30.0

let coords row col =
  float(col) * scale + 0.5, float(row) * scale + 0.5

let rectangle row col =
  let x, y = coords row col
  x + 1.0, y + 1.0, scale - 1.0, scale - 1.0

// Cairo's ShowText places the origin at the bottom left
let letterPos row col =
  let x, y = coords row col
  x + 10.0, y + scale - 2.0

let numberPos row col =
  let x, y = coords row col
  x + 1.0, y + 10.0

// Colors
let black = Color(0.0, 0.0, 0.0)
let white = Color(1.0, 1.0, 1.0)
let blue = Color(0.8, 0.8, 1.0)

let DrawGrid(cr : Cairo.Context, state) =
  let rows = state.xword.rows
  let cols = state.xword.cols
  let grid = state.xword.grid

  cr.SelectFontFace ("Sans", FontSlant.Normal, FontWeight.Normal)
  cr.LineWidth <- 1.0
  cr.SetSourceColor(Color(0.0, 0.0, 0.0))

  for row = 0 to rows do
    let x, y = coords row cols
    cr.MoveTo(0.0, y)
    cr.LineTo(x, y)

  for col = 0 to cols do
    let x, y = coords rows col
    cr.MoveTo(x, 0.0)
    cr.LineTo(x, y)

  cr.Stroke ()

  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      let cell = grid.[row, col].cell
      let num = grid.[row, col].num
      let x, y = coords row col

      let color =
        match cell with
        | Black -> black
        | _ -> white

      cr.SetSourceColor(color)
      cr.Rectangle(x + 0.5, y + 0.5, scale - 1.0, scale - 1.0)
      cr.Fill()

      cr.SetSourceColor(black)
      cr.SetFontSize(20.0)
      match cell with
      | Letter s ->
          let x1, y1 = letterPos row col
          cr.MoveTo(x1, y1)
          cr.ShowText(s)
      | _ -> ()

      cr.SetFontSize(8.0)
      if num > 0 then
        let x1, y1 = numberPos row col
        cr.MoveTo(x1, y1)
        cr.ShowText(string(num))


let Run (state) =
  Application.Init ()
  let window = new Window ("helloworld")

  window.SetDefaultSize(width, height)
  window.DeleteEvent.Add(fun e -> window.Hide(); Application.Quit(); e.RetVal <- true)

  let drawing = new Gtk.DrawingArea () 
  drawing.Drawn.Add(fun args ->
    let cr = args.Cr
    use target = cr.GetTarget ()
    use grid = target.CreateSimilar (Content.ColorAlpha, width, height)
    use cr_grid = new Context(grid)
    DrawGrid(cr_grid, state)
    cr.SetSourceSurface(grid, 10, 10)
    cr.Paint ()
    )
  window.Add(drawing)
  window.ShowAll()
  window.Show()
  Application.Run ()
