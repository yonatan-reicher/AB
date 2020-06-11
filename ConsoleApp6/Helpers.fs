[<AutoOpen>]
module Asmb.Helpers


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