[<AutoOpen>]
module Asmb.TopLevel

type Label = 
    Local of string | Gloabal of string
    override t.ToString() =
        match t with 
        | Local s -> sprintf "@@%s" s
        | Gloabal s -> s

type Size = 
    Byte | Word | DWord
    override t.ToString () = 
        match t with 
        | Byte -> "byte" 
        | Word -> "word"
        | DWord -> "dword"

module Size = 
    let max x y = 
        match x, y with
        | DWord, _ | _, DWord -> DWord
        | Word, _ | _, Word -> Word
        | _ -> Byte
    //  Bytes are stored as words on the stack... sorry
    let bytes = function Byte -> 2 | Word -> 2 | DWord -> 4

type uint = uint32

type Literal = 
    | UInt of uint
    | Char of char
    | Offset of string
    override t.ToString() =
        match t with
        | UInt i -> string (int i)
        | Char c -> string c
        | Offset id -> sprintf "offset %s" id

module Literal =
    let size = function
        | UInt i when uint32 (uint8 i) = i ->
            Byte
        | UInt i when uint32 (uint16 i) = i ->
            Word
        | UInt _ -> DWord
        | Char _ -> Byte
        | Offset _ -> Word

type SizeContext = {Vars: Map<string, Size>; Procs: Map<string, Size*Size list>}
