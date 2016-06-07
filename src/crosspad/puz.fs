module Puz

open System
open System.IO
open System.Text
open BitSyntax
open Xword

(* extension read in from binary *)
type extension = {
  section: string;
  length: int;
  data: string;
  checksum: int;
}

(* puzzle read in from binary *)
type puzzle = {
  preamble: string;
  postscript: string;
  title: string;
  author: string;
  copyright: string;
  width: int;
  height: int;
  n_clues: int;
  version: string;
  scrambled_checksum: int;
  fill: string;
  solution: string;
  clues: string list;
  notes: string;
  extensions: extension list;
  puzzle_type: int;
  solution_state: int;
  scrambled_tag: int;
}

let new_puzzle = {
  preamble = "";
  postscript = "";
  title = "";
  author = "";
  copyright = "";
  width = 0;
  height = 0;
  n_clues = 0;
  version = "1.3";
  scrambled_checksum = 0;
  fill = "";
  solution = "";
  clues = [];
  notes = "";
  extensions = [];
  puzzle_type = 1;
  solution_state = 0;
  scrambled_tag = 0;
}

let read_string (sr : StreamReader) =
  let rec read (sr : StreamReader) (sb : StringBuilder) =
    let c = sr.Read()
    match c with
    | 0 | -1 -> sb.ToString()
    | _ -> read sr (sb.Append (char c))

  let sb = new StringBuilder ()
  let encoding = new System.Text.ASCIIEncoding ()
  let out = read sr sb
  Console.WriteLine("Read string {0}", out)
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
  { new_puzzle with
      preamble = preamble;
      width = width; height = height;
      version = version; n_clues = n_clues;
      solution = solution; fill = fill
  }

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
  let make_clue s = { answer = ""; clue = s; edited_clue = s }
  let mutable i = 0
  Xword.renumberWithCallbacks
    (fun n -> ac.Add(make_clue cs.[i]); i <- i + 1)
    (fun n -> dn.Add(make_clue cs.[i]); i <- i + 1)
    xw

let read (s : Stream) =
  let puz = read_header s 0
  let encoding = System.Text.Encoding.GetEncoding("ISO-8859-1")
  let sr = new StreamReader(s, encoding)
  let title = read_string sr
  let author = read_string sr
  let copyright = read_string sr
  System.Console.WriteLine(">> n_clues: {0}", puz.n_clues)
  let clues = Array.init puz.n_clues (fun i -> read_string sr)
    
  let xw = make_xword(puz.height, puz.width)
  unpack_solution xw puz.solution
  unpack_clues xw clues |> ignore
  xw
