module EtoGui

open Eto.Forms
open Eto.Drawing
open Xword

let Run (state) =
  let xw = state.xword
  let rows = xw.rows
  let cols = xw.cols
  let cursor = state.cursor

  let app = new Application()
  let d = new Drawable()

  let blackBrush = new SolidBrush(Colors.Black)
  let whiteBrush = new SolidBrush(Colors.White)
  let cursorBlackBrush = new SolidBrush(Colors.DarkGreen)
  let cursorWhiteBrush = new SolidBrush(Colors.LightGreen)
  let cursorSymBlackBrush = new SolidBrush(Colors.DarkSeaGreen)
  let cursorSymWhiteBrush = new SolidBrush(Colors.LightSeaGreen)
  let arial = new FontFamily("Arial")
  let letterFont = new Font(arial, 14.f)
  let numberFont = new Font(arial, 6.f)
  let scale = 30.f

  let coords row col =
    single(col) * scale, single(row) * scale

  let rectangle row col =
    let x, y = coords row col
    new RectangleF(x + 1.f, y + 1.f, scale - 1.f, scale - 1.f)

  let letterPos row col =
    let x, y = coords row col
    new PointF(x + 10.f, y + 10.f)
  
  let numberPos row col =
    let x, y = coords row col
    new PointF(x + 1.f, y + 1.f)

  let cellBg (row : int) (col : int) (cursor : Cursor) (cell : cell) =
    let is_cursor = row = cursor.Y && col = cursor.X
    let is_sym_cursor = cursor.IsSymmetric(col, row)
    match cell, is_cursor, is_sym_cursor with
    | Black, true, _ -> cursorBlackBrush
    | Black, _, true -> cursorSymBlackBrush
    | Black, _, _ -> blackBrush
    | _, true, _ -> cursorWhiteBrush
    | _, _,true -> cursorSymWhiteBrush
    | _, _, _ -> whiteBrush

  d.Paint.Add(fun e ->
    let g = e.Graphics
    g.TranslateTransform(100.f, 100.f)

    for row = 0 to rows do
      let x, y = coords row cols
      g.DrawLine(Colors.Black, 0.f, y, x, y)

    for col = 0 to cols do
      let x, y = coords rows col
      g.DrawLine(Colors.Black, x, 0.f, x, y)

    for row = 0 to rows - 1 do
      for col = 0 to cols - 1 do
        let cell = xw.grid.[row, col].cell
        let num = xw.grid.[row, col].num

        let brush = cellBg row col cursor cell 
        g.FillRectangle(brush, rectangle row col)
        
        match cell with
        | Letter c ->
          g.DrawText(letterFont, blackBrush, letterPos row col, c)
        | _ -> ()

        if num > 0 then
          g.DrawText(numberFont, blackBrush, numberPos row col, string num)
    )

  let f = new Form(Topmost=true, ClientSize = new Size(600, 480))

  f.KeyDown.Add(fun e ->
    System.Console.WriteLine(">> {0}", e.KeyData)

    match e.Key with
    | Keys.Up -> cursor.Move(Move_Up)
    | Keys.Down -> cursor.Move(Move_Down)
    | Keys.Left -> cursor.Move(Move_Left)
    | Keys.Right -> cursor.Move(Move_Right)
    | Keys.Space -> toggleBlack xw cursor
    | Keys.A -> toggleBlack xw cursor
    | _ -> if e.IsChar then setLetter xw cursor (string e.KeyChar)

    d.Invalidate())

  f.Content <- d
  f.Show()
  app.Run()
