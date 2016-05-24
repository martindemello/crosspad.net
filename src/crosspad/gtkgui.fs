module GtkGui

open System
open Gtk
open Cairo
open Xword

let (width, height) = (800, 600)

let (xoff, yoff) = (10, 10)

let scale = 30.0

let coords row col =
  float(col) * scale + 0.5, float(row) * scale + 0.5

let rectangle row col =
  let x, y = coords row col
  x + 0.5, y + 0.5, scale - 1.0, scale - 1.0

let fromMouseCoords(x, y) =
  let col = int((x - float(xoff)) / scale)
  let row = int((y - float(yoff)) / scale)
  (row, col)

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
let blue_a = Color(0.75, 0.75, 1.0, 0.5)
let red_a = Color(1.0, 0.75, 0.75, 0.5)
let dark_green_a = Color(0.4, 0.8, 0.4, 0.5)

// Keys
let isAscii k =
  (k >= Gdk.Key.A && k <= Gdk.Key.Z) ||
  (k >= Gdk.Key.a && k <= Gdk.Key.z)

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

      let color =
        match cell with
        | Black -> black
        | _ -> white

      cr.SetSourceColor(color)
      let (x, y, w, h) = rectangle row col
      cr.Rectangle(x, y, w, h)
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

let DrawCursor(cr : Cairo.Context, state) =
  let cursor = state.cursor
  let col, row = cursor.X, cursor.Y
  let (x, y, w, h) = rectangle row col
  let color = match cursor.Dir with Across -> blue_a | Down -> red_a
  cr.SetSourceColor(color)
  cr.Rectangle(x, y, w, h)
  cr.Fill()
  
  let col', row' = state.cursor.Symmetric()
  let (x, y, w, h) = rectangle row' col'
  cr.SetSourceColor(dark_green_a)
  cr.Rectangle(x, y, w, h)
  cr.Fill()

type MyWin(arg: String) =
  class
    inherit Gtk.Window(arg)

    let mutable on_key_fn = fun e -> true

    member this.OnKeyPress
      with public get() = on_key_fn
      and public set fn = on_key_fn <- fn

    override this.OnKeyPressEvent(e : Gdk.EventKey) =
      if this.OnKeyPress(e) then
        base.OnKeyPressEvent(e)
      else
        false

  end

let Run (state) =
  Application.Init ()
 
  let cursor = state.cursor
  let xw = state.xword

  let window = new MyWin ("helloworld")
  window.SetDefaultSize(width, height)
  window.DeleteEvent.Add(fun e ->
    window.Hide()
    Application.Quit()
    e.RetVal <- true)

  let drawing = new Gtk.DrawingArea () 
  drawing.AddEvents(int(Gdk.EventMask.ButtonPressMask))

  drawing.Drawn.Add(fun args ->
    let cr = args.Cr
    use target = cr.GetTarget ()
    use grid = target.CreateSimilar (Content.ColorAlpha, width, height)
    use cur = target.CreateSimilar (Content.ColorAlpha, width, height)
    use cr_grid = new Context(grid)
    DrawGrid(cr_grid, state)
    cr.SetSourceSurface(grid, xoff, yoff)
    cr.Paint ()
    use cr_cur = new Context(cur)
    DrawCursor(cr_cur, state)
    cr.SetSourceSurface(cur, xoff, yoff)
    cr.Paint ()
    )

  drawing.ButtonPressEvent.Add(fun args ->
    let ev = args.Event
    let (row, col) = fromMouseCoords(ev.X, ev.Y)
    cursor.MoveTo(col, row)
    window.QueueDraw ()
    )

  window.OnKeyPress <- (fun e ->
    match e.Key with
    | Gdk.Key.Up -> cursor.Move(Move_Up)
    | Gdk.Key.Down -> cursor.Move(Move_Down)
    | Gdk.Key.Left -> cursor.Move(Move_Left)
    | Gdk.Key.Right -> cursor.Move(Move_Right)
    | Gdk.Key.Tab -> cursor.FlipDir()
    | Gdk.Key.space -> toggleBlack xw cursor
    | Gdk.Key.BackSpace -> deleteLetter xw cursor true 
    | Gdk.Key.Delete -> deleteLetter xw cursor false
    | k when isAscii(k) -> setLetter xw cursor ((string k).ToUpper())
    | _ -> Console.WriteLine(">> #{0}", e.Key)
    window.QueueDraw ()
    true
    )
    
  window.Add(drawing)
  window.ShowAll()
  window.Show()
  Application.Run ()
