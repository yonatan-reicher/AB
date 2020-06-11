[<AutoOpen>]
module Asmb.TopLevel

type Label = 
    Local of string | Gloabal of string
    override t.ToString() =
        match t with 
        | Local s -> sprintf "@@%s" s
        | Gloabal s -> s

type Size = 
    Void | Byte | Word | DWord
    override t.ToString () = 
        match t with 
        | Void -> "void"
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
    let bytes = function Void -> 0u | Byte -> 2u | Word -> 2u | DWord -> 4u

type uint = uint32

type Literal = 
    | UInt of uint
    | Char of char
    | Offset of string
    override t.ToString() =
        match t with
        | UInt i -> string (int i)
        | Char c -> sprintf "'%c'" c
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

type FuncSig = Size * Size list

type FuncBuilder() =
    member _.Bind(x: 'a -> 'a, f) = x >> f()
    member _.Bind(x: ('a->'a)->('a->'a), f) = x <| f()
    member _.Bind(x: ('b->('a->'a))->('a->'a), f) = x f
    member _.Zero() = id
    member _.For(xs: seq<'b>, f: 'b -> 'a -> 'a) = fun a -> Seq.fold (fun a b -> f b a) a xs
    member _.Combine(x, y) = x >> y
    member _.Delay x = x ()
let func = FuncBuilder()

type CatchBuilder() =
    member _.Bind(x: Result<'a,'err>, f: 'a->Result<'b,'err>) = Result.bind f x
    member inline _.Zero(): Result< ^a, 'b > = 
        Ok LanguagePrimitives.GenericZero< ^a >
    member _.Delay x: 'a = x()
    member _.Combine(x, y: Result<'a,'err>) = Result.bind (fun () -> y) x
    member _.Combine(x: Result<'a,'err>, y) = Result.bind (fun () -> x) y
    member _.Combine(x: Result<'a,'err>, y: Result<'b,'err>) =
        match x, y with
        | Ok x, Ok y -> Ok (x, y)
        | Error e, _ | _, Error e -> Error e
    member _.Return (x: 'a) = Ok x
    member _.ReturnFrom x: 'a = x
    member _.Yield (x: 'a) = Ok x
    member _.YieldFrom x: 'a = x
    member _.For(xs: 'a seq, f: 'a->Result<'b,'err>): Result<'b seq, 'err> = 
        let mutable res = Ok []
        while (match res with Ok _ -> true | Error _ -> false) && not <| Seq.isEmpty xs do
            let x = Seq.head xs
            match f x with
            | Error e -> res <- Error e
            | Ok x -> res <- Ok (x :: match res with Ok res -> res)
        Result.map Seq.rev res
let catch = CatchBuilder()
        
module Result =
    let map2 f x y = catch {let! x = x in let! y = y in return f x y}