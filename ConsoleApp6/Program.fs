// Learn more about F# at http://fsharp.org

open System
open System.Diagnostics
open System.IO

open FParsec
open Asmb
open Asmb.AST
open Asmb.AST.Parsing
open Asmb.IL
open Asmb.IL.Optimization
open Asmb.IL.Write
open Asmb.Translate

/// Compiles asmb code from a given source file from the TASM folder. fileName is the name of the file in th TASM folder 
let compileAndRun debug (optimize: bool) (fileName: string) =
    let fileName = if fileName.EndsWith ".ab" then fileName.[0..fileName.Length-1-3] else fileName
    let sourceCode = File.ReadAllText (sprintf @"C:\TASM\%s.ab" fileName)
    match run pprogram sourceCode with
    | Failure (error, _, _) -> printfn "%s" error
    | Success (asmbProgram, (), _) ->
        let program = translateProg asmbProgram |> if optimize then optimizeProgram else id
        let str = writeProg program     
        printfn "%s" str
        File.WriteAllText(fileName + ".asm", str)
        let p = System.Diagnostics.Process.Start(@"C:\Program Files (x86)\DOSBox-0.74-3\DOSBox.exe", sprintf @"-c ""TASM %s"" -c ""TLINK %s"" -c ""%s""" (fileName + ".asm") fileName (if debug then "TD " + fileName else fileName))
        p.WaitForExit()
        p.Dispose()
        
//let runAssembly

[<EntryPoint>]
let rec main args = 
    match args with
    | [|fileName; "-debug"|] ->
        compileAndRun true true fileName
    | [|fileName|] ->
        compileAndRun false true fileName
    | _ -> printfn "Incorrect arguments! \nUse: asmb file-name OR asmb file-name -debug"
    Console.ReadKey true |> ignore
    0
