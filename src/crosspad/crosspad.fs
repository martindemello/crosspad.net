module crosspad

open Eto.Forms
open Eto.Drawing

[<EntryPoint>]
let main argv =
  let app = new Application()
  let d = new Drawable()

  d.Paint.Add(fun e ->
    let g = e.Graphics
    g.TranslateTransform(100.f, 100.f)
    g.DrawEllipse(Colors.Blue, 0.f, 0.f, 300.f, 300.f)
  )

  let f = new Form(Topmost=true, ClientSize = new Size(600, 480))
  f.Content <- d
  f.Show()
  app.Run()
  0
