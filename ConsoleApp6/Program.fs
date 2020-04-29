// Learn more about F# at http://fsharp.org

open System

open FParsec
open Asmb
open Asmb.AST
open Asmb.AST.Parsing
open Asmb.IL
open Asmb.IL.Optimization
open Asmb.IL.Write
open Asmb.Translate

let handleSourceCode = 
    run pprogram
    >>  function
        | Success (a,(),_) ->
            let b = 
                a
                |> translateProg
                |> optimizeProgram
                |> writeProg
                |> fun s -> s.Replace("\t", "    ")
            printfn "Compilation done successfuly!"
            printfn "Would you like to see dump? \n1 - tree \n2 - file \n3 - both \nother - none"
            match Console.ReadLine () with
            | "1" -> printfn "Tree: \n%O" a
            | "2" -> printfn "File: \n%s" b
            | "3" -> printfn "Tree: \n%O \n\nFile: \n%s" a b
            | _ -> printfn "Fine! enjoy your .asm file"
        | Failure (a,b,c) ->
            printfn "%O \n%O \n%O" a b c

[<EntryPoint>]
let rec main args = 
    match args with
    | [||] ->
        printfn "File name or 2:dont or 3:awesome"
        let fileName = Console.ReadLine ()
        match fileName with
        | "dont" | "2" ->
            let rec readLines () = let line = Console.ReadLine () in if line = ";;" then "" else sprintf "%s\n%s" line (readLines ())
            printfn "Fine. Then input your source code:"
            readLines ()
            |> handleSourceCode
        | "awesome" | "3" ->
            let code = @"
func word main() {
    return fib(12);
}

func word fib(x word) { 
    if x = 1 {
        return 0;
    } else {
        if x = 2 {
            return 1;
        } else {
            return fib(x - 1) + fib(x - 2);
        };   
    };
}"
            printfn "Code: \n%s" code
            handleSourceCode code
        | _ -> ignore(main [|fileName|])
    | [|fileName|] ->
        let fileName = if fileName.EndsWith ".ab" then fileName.[0..fileName.Length-1-3] else fileName  //  Trim the extension
        IO.File.ReadAllText (sprintf "./%s.ab" fileName)
        |> handleSourceCode
    Console.ReadKey true |> ignore
    0
