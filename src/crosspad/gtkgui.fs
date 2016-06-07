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

let DrawXword(cr : Cairo.Context, state) =
  use target = cr.GetTarget ()
  use grid = target.CreateSimilar (Content.ColorAlpha, width, height)
  use cur = target.CreateSimilar (Content.ColorAlpha, width, height)
  cr.SetSourceColor(white)
  cr.Rectangle(0.0, 0.0, float(width), float(height))
  cr.Fill()
  use cr_grid = new Context(grid)
  DrawGrid(cr_grid, state)
  cr.SetSourceSurface(grid, xoff, yoff)
  cr.Paint ()
  use cr_cur = new Context(cur)
  DrawCursor(cr_cur, state)
  cr.SetSourceSurface(cur, xoff, yoff)
  cr.Paint ()

// Clues
type ClueWidget(clues: clues) as this =
  class
    inherit Gtk.VBox(true, 0)

    let clues = clues
    let tree = new Gtk.TreeView()
    let model = new Gtk.ListStore(typeof<clue>)
    let clue_col = new Gtk.TreeViewColumn()

    let render_cell (fn : clue -> string)
                    (column : Gtk.TreeViewColumn)
                    (cell : Gtk.CellRenderer)
                    (model : Gtk.ITreeModel)
                    (iter : Gtk.TreeIter) =
      let clue = model.GetValue (iter, 0) :?> clue
      (cell :?> Gtk.CellRendererText).Text <- (fn clue)

    let render_clue = render_cell (fun clue -> clue.clue)
    let render_answer = render_cell (fun clue -> clue.answer)

    let make_column(title, data_func) =
      let col = new Gtk.TreeViewColumn()
      let renderer = new Gtk.CellRendererText ()
      let index = tree.AppendColumn(col) - 1
      col.Title <- title
      col.PackStart (renderer, true);
      col.SetCellDataFunc (renderer, new Gtk.TreeCellDataFunc (data_func))
      col

    let mutable on_change_fn = fun clue -> true

    let init () =
      let answer_col = make_column("Light", render_answer)
      let clue_col = make_column("Clue", render_clue)

      for clue in clues.across do
        model.AppendValues(clue) |> ignore

      tree.CursorChanged.Add(fun (e : EventArgs) ->
          let selection = tree.Selection
          let mutable iter = new TreeIter ()
          if selection.GetSelected(&iter) then
            let clue = model.GetValue(iter, 0) :?> clue
            Console.WriteLine("{0} : {1}", clue.answer, clue.clue)
            on_change_fn clue |> ignore
          )

      tree.Model <- model
      this.PackStart(tree, true, true, 0u)

    do init ()

    member this.OnChange
      with public get() = on_change_fn
      and public set fn = on_change_fn <- fn

    member this.Refresh () =
      tree.QueueDraw()

  end

// Drawing area
type XwordWidget(state) as this =
  class
    inherit Gtk.DrawingArea()

    let state = state
    let xw = state.xword
    let cursor = state.cursor

    let init () =
      this.AddEvents(int(Gdk.EventMask.ButtonPressMask))
      this.CanFocus <- true

    do init ()

    override this.OnGetPreferredHeight(min_height : byref<int>, natural_height : byref<int>) =
      min_height <- 300
      natural_height <- int(scale) * xw.rows + 20

    override this.OnGetPreferredWidth(min_width : byref<int>, natural_width : byref<int>) =
      min_width <- 300
      natural_width <- int(scale) * xw.cols + 20

    override this.OnDrawn(cr : Cairo.Context) =
      DrawXword(cr, state)
      true

    override this.OnKeyPressEvent(e : Gdk.EventKey) =
      let mutable handled = true
      match e.Key with
      | Gdk.Key.Up -> cursor.Move(Move_Up)
      | Gdk.Key.Down -> cursor.Move(Move_Down)
      | Gdk.Key.Left -> cursor.Move(Move_Left)
      | Gdk.Key.Right -> cursor.Move(Move_Right)
      | Gdk.Key.Page_Up | Gdk.Key.Next (* PgDn *) -> cursor.FlipDir()
      | Gdk.Key.space -> toggleBlack xw cursor
      | Gdk.Key.BackSpace -> deleteLetter xw cursor true
      | Gdk.Key.Delete -> deleteLetter xw cursor false
      | k when isAscii(k) -> setLetter xw cursor ((string k).ToUpper())
      | _ -> Console.WriteLine(">> #{0}", e.Key); handled <- false

      if handled then
        this.QueueDraw()
        true
      else
        base.OnKeyPressEvent(e)

    override this.OnButtonPressEvent(e : Gdk.EventButton) =
      let (row, col) = fromMouseCoords(e.X, e.Y)
      cursor.MoveTo(col, row)
      this.QueueDraw()
      this.GrabFocus()
      true
  end

// Clue entry
type ClueEntryWidget () as this =
  class
    inherit Gtk.TextView ()

    let mutable clue = {answer = ""; clue = ""; edited_clue = ""}

    let init () =
      this.Buffer.Changed.Add(fun e -> clue.edited_clue <- this.Buffer.Text)

    do init ()

    override this.OnGetPreferredHeight(min_height : byref<int>, natural_height : byref<int>) =
      min_height <- 24
      natural_height <- 50

    member this.Clue
      with public get() = clue
      and public set clue' = clue <- clue'

    member this.Update () =
      this.Buffer.Text <- clue.edited_clue
  end

let Run (state) =
  Application.Init ()

  let window = new Gtk.Window("helloworld")
  window.SetDefaultSize(width, height)
  window.DeleteEvent.Add(fun e ->
    window.Hide()
    Application.Quit()
    e.RetVal <- true)

  let grid = new XwordWidget(state)
  let clues = new ClueWidget(state.xword.clues)
  let current_clue = new ClueEntryWidget ()
  let commit_clue = new Gtk.Button("Save")
  let cc_box = new Gtk.HBox(false, 1)
  cc_box.PackStart(current_clue, true, true, 1u)
  cc_box.PackStart(commit_clue, false, true, 1u)

  clues.OnChange <- (fun clue ->
    current_clue.Clue <- clue
    current_clue.Update ()
    false
    )

  current_clue.Editable <- true
  commit_clue.Clicked.Add(fun e ->
    current_clue.Clue.clue <- current_clue.Clue.edited_clue
    clues.Refresh()
    )
  let hbox = new Gtk.HBox(false, 1)
  let vbox = new Gtk.VBox(false, 1)
  hbox.PackStart(grid, false, true, 1u)
  hbox.PackStart(clues, true, true, 1u)
  vbox.PackStart(hbox, false, true, 1u)
  vbox.PackStart(cc_box, false, true, 1u)
  window.Add(vbox)
  window.ShowAll()
  window.Show()
  Application.Run ()
