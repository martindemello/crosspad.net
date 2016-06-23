module Qxw

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open Sscanf
open Xword

let encoding = System.Text.Encoding.GetEncoding("ISO-8859-1")

exception PuzzleFormatError of string

type puzzle = {
  height : int
  width : int
  symmr : int
  symmm : int
  symmd : int
}

let fail () = raise (PuzzleFormatError "File format error")

let check p =
  if not p then fail ()

type LineScanner(s : Stream) =
  class
    let sr = new StreamReader(s)
    let mutable line = sr.ReadLine()

    let read_n f pf =
      let rec read acc =
        match f pf with
        | Some k -> read (k :: acc)
        | None -> List.rev acc
      read []

    member this.NextLine () =
      line <- sr.ReadLine()

    member this.TryScan (pf:PrintfFormat<_,_,_,_,'t>) =
      try
        let value = sscanf pf line
        this.NextLine()
        Some value
      with
      | ScanfFailure (a, b) -> None

    member this.Scan (pf:PrintfFormat<_,_,_,_,'t>) =
      let ret = sscanf pf line
      this.NextLine()
      ret

    member this.TryScanKv (pf:PrintfFormat<_,_,_,_,'t>) =
      match (this.TryScan pf) with
      | Some _ -> Some (this.Scan "+%s")
      | None -> None
    
    member this.ReadKv (key : string) =
      match (line = key) with
      | true ->
          this.NextLine()
          let v = this.Scan "+%s"
          unbox<string> v
      | false -> fail ()

    member this.ScanWhile (pf:PrintfFormat<_,_,_,_,'t>) =
      read_n this.TryScan pf
    
    member this.ScanKvWhile (pf:PrintfFormat<_,_,_,_,'t>) =
      read_n this.TryScanKv pf
  end

let read_version (s : LineScanner) =
  let ver, _ = s.Scan "#QXW2v%d %s"
  unbox<int> ver
  
let read_gp (s : LineScanner) =
  let gtype, width, height, symmr, symmm, symmd =
    s.Scan "GP %d %d %d %d %d %d"

  if unbox<int>(gtype) <> 0 then failwith "Only square grids are supported"
  (unbox<int> height, unbox<int> width)

let read (stream : Stream) =
  let s = new LineScanner(stream)
  try
    let version = read_version s
    let h, w = read_gp s
    let title = s.ReadKv "TTL"
    let author = s.ReadKv "AUT"
    let _ = s.Scan "GLP %d %d %d %d %d"
    let _ = s.ScanWhile "GSP %x %x %d %d %d %d %x"
    let _ = s.ScanKvWhile "GSPMK %d"
    let _ = s.ScanKvWhile "TM %d %d %d %d %d"
    let _ = s.ScanKvWhile "TMSG %d %d"
    let _ = s.ScanWhile "TCST %d %d %s"
    let _ = s.ScanKvWhile "DFN %d"
    let _ = s.ScanKvWhile "DSF %d"
    let _ = s.ScanKvWhile "DAF %d"
    let sq = s.ScanWhile "SQ %d %d %d %d %d %c"
    let sqsp = s.ScanWhile "SQSP %d %d %x %x %d %d %d %d %x"
    let sqspmk = s.ScanKvWhile "SQSPMK %d %d %d"
    let sqlp = s.ScanWhile "SQLP %d %d %d %d %d %d %d %d"
    let _ = s.ScanWhile "VL %d %d %d %d"
    let _ = s.ScanWhile "VLP %d %d %d %d %d %d"
    let sqct = s.ScanWhile "SQCT %d %d %d %s"
    Console.WriteLine("{0}", (h, w, title))
  with
  | ScanfFailure (a, b) ->
      Console.WriteLine("Could not match '{0}' with '{1}'", a, b)
