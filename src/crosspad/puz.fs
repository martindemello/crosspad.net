module Puz

open System
open System.IO
open System.Text
open BitSyntax
open Xword

let encoding = System.Text.Encoding.GetEncoding("ISO-8859-1")

(* extension read in from binary *)
type extension = {
  section: string
  length: int
  data: string
  checksum: int
}

(* puzzle read in from binary *)
type puzzle = {
  preamble: string
  postscript: string
  title: string
  author: string
  copyright: string
  width: int
  height: int
  n_clues: int
  version: string
  scrambled_checksum: int
  fill: string
  solution: string
  clues: string list
  notes: string
  extensions: extension list
  puzzle_type: int
  solution_state: int
  scrambled_tag: int
}

let new_puzzle = {
  preamble = ""
  postscript = ""
  title = ""
  author = ""
  copyright = ""
  width = 0
  height = 0
  n_clues = 0
  version = "1.3"
  scrambled_checksum = 0
  fill = ""
  solution = ""
  clues = []
  notes = ""
  extensions = []
  puzzle_type = 1
  solution_state = 0
  scrambled_tag = 0
}

let read_string (sr : StreamReader) =
  let rec read (sr : StreamReader) (sb : StringBuilder) =
    let c = sr.Read()
    match c with
    | 0 | -1 -> sb.ToString()
    | _ -> read sr (sb.Append (char c))

  let sb = new StringBuilder ()
  let out = read sr sb
  out
    
let read_header (stream : Stream) start =
  let preamble, width, height, version, n_clues, solution, fill =
    bitReader stream {
       let! preamble = BitReader.ReadString(start)
       let! checksum = BitReader.ReadBytes(2)
       let! magic = BitReader.ReadString(0xc)
       let! checksum_cib = BitReader.ReadInt16()
       let! checksum_low = BitReader.ReadInt32()
       let! checksum_high = BitReader.ReadInt32()
       let! version = BitReader.ReadString(4)
       let! reserved1c = BitReader.ReadInt16()
       let! scrambled_checksum = BitReader.ReadInt16()
       let! reserved20 = BitReader.ReadString(0xc)
       let! width = BitReader.ReadInt32(numBits = 8)
       let! height = BitReader.ReadInt32(numBits = 8)
       let! n_clues = BitReader.ReadInt32(numBits = 16)
       let! puzzle_type = BitReader.ReadInt16()
       let! scrambled_tag = BitReader.ReadInt16()
       let n = width * height
       let! solution = BitReader.ReadString(n)
       let! fill = BitReader.ReadString(n)
       return preamble, width, height, version, n_clues, solution, fill
    }
  (* BitSyntax consumes too much of the stream, so we need to rewind to the
   * end of the read portion *)
  stream.Position <- int64(0x34 + 2 * width * height)
  { new_puzzle with
      preamble = preamble;
      width = width; height = height;
      version = version; n_clues = n_clues;
      solution = solution; fill = fill
  }

let read_extension (stream : Stream) =
  let pos = stream.Position
  let section, length, checksum, data =
    bitReader stream {
      let! section = BitReader.ReadString(4)
      let! length = BitReader.ReadInt32(numBits = 16)
      let! checksum = BitReader.ReadInt32(numBits = 16)
      let! data = BitReader.ReadString(length + 1)
      return section, length, checksum, data
    }
  stream.Position <- pos + int64(8 + length + 1)
  { data = data; section = section; length = length; checksum = checksum }

let read_extensions (stream : Stream) =
  let rec read stream out =
    let ext = read_extension stream
    match ext.length with
    | 0 -> List.rev out
    | _ -> read stream (ext :: out)
  read stream []

let cell_of_char c =
  match c with
  | '.' -> Black
  | ' ' -> Empty
  | c  -> Letter (string c)

let unpack_solution xw (s : string) =
  for y = 0 to xw.rows - 1 do
    for x = 0 to xw.cols - 1 do
      let ix = y * xw.cols + x in
      let cell = cell_of_char s.[ix] in
      Xword.setCell xw x y cell
    done
  done
 
let unpack_clues (xw : xword) (cs : string []) =
  let ac = xw.clues.across
  let dn = xw.clues.down
  let make_clue (l : light) s = { light = l; clue = s; edited_clue = s }
  let mutable i = 0
  Xword.renumberWithCallbacks
    (fun light -> ac.Add(make_clue light cs.[i]); i <- i + 1)
    (fun light -> dn.Add(make_clue light cs.[i]); i <- i + 1)
    xw

let read (s : Stream) =
  let sr = new StreamReader(s, encoding, false)

  let puz = read_header s 0

  let stream = s
  let pos = s.Position
  let title = read_string sr
  let author = read_string sr
  let copyright = read_string sr
  let clues = Array.init puz.n_clues (fun i -> read_string sr)
  let notes = read_string sr

  // TODO: We should not be mixing reads from a StreamReader and an
  // underlying Stream. Just slurp the file into a string and refactor
  // all this hackery.
  let s1 = List.map String.length [title; author; copyright; notes]
  let s2 = List.map String.length (List.ofArray clues)
  let s3 = (List.sum s1) + (List.sum s2) + (List.length s1) + (List.length s2)
  s.Position <- pos + int64(s3)

  let extensions = read_extensions s
  Console.WriteLine("Extensions: {0}", List.map (fun x -> x.section) extensions)
    
  let xw = make_xword(puz.height, puz.width)
  unpack_solution xw puz.solution
  unpack_clues xw clues |> ignore
  xw
