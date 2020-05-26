open System
open System.Diagnostics
open System.IO

open FParsec
open Asmb.AST.Parsing
open Asmb.IL.Optimization
open Asmb.IL.Write
open Asmb.Translate

module Interop = 
    open System.Runtime.InteropServices

    type CtrlType =
        | CTRL_C_EVENT = 0
        | CTRL_BREAK_EVENT = 1
        | CTRL_CLOSE_EVENT = 2
        | CTRL_LOGOFF_EVENT = 5
        | CTRL_SHUTDOWN_EVENT = 6

    // https://msdn.microsoft.com/fr-fr/library/windows/desktop/ms683242.aspx
    type SetConsoleCtrlEventHandler = delegate of CtrlType -> bool

    // https://msdn.microsoft.com/fr-fr/library/windows/desktop/ms686016.aspx
    [<DllImport("Kernel32")>]
    extern bool SetConsoleCtrlHandler(SetConsoleCtrlEventHandler handler, bool add)

    let mutable private cleanUp = []
    let handler: SetConsoleCtrlEventHandler =
        //SetConsoleCtrlEventHandler(function
        //    | CtrlType.CTRL_BREAK_EVENT
        //    | CtrlType.CTRL_C_EVENT
        //    | CtrlType.CTRL_LOGOFF_EVENT
        //    | CtrlType.CTRL_SHUTDOWN_EVENT
        //    | CtrlType.CTRL_CLOSE_EVENT ->
        //        // TODO Cleanup resources
        //        for c in cleanUp do c()
        //        Environment.Exit(0);
        //        false
        //    | _ -> false)
        SetConsoleCtrlEventHandler(fun _ -> (for c in cleanUp do c()); false)
    let onExit x = cleanUp <- x :: cleanUp

    do
        SetConsoleCtrlHandler(handler, true) |> ignore


/// Compiles asmb code from a given source file from the TASM folder. fileName is the name of the file in th TASM folder 
let compile (optimize: bool) (fileName: string) =
    let fileName = if fileName.EndsWith ".ab" then fileName.[0..fileName.Length-1-3] else fileName
    let sourceCode = File.ReadAllText (sprintf @"C:\TASM\%s.ab" fileName)
    match run pprogram sourceCode with
    | Failure (error, _, _) -> printfn "%s" error
    | Success (asmbProgram, (), _) ->
        let program = translateProgram asmbProgram |> if optimize then optimizeProgram else id
        let str = writeProg program     
        printfn "%s" str
        File.WriteAllText(sprintf "C:\TASM\%s.asm" fileName, str)
        
        use p = System.Diagnostics.Process.Start(@"C:\Program Files (x86)\DOSBox-0.74-3\DOSBox.exe", sprintf @"-c ""TASM %s"" -c ""TLINK %s""" fileName fileName)
        Interop.onExit p.Kill
        p.WaitForExit()

[<EntryPoint>]
let rec main args = 
    match args with
    | [|fileName|] ->
        compile true fileName
    | _ -> printfn "Incorrect arguments! \nUse: asmb file-name"
    Console.ReadKey true |> ignore
    0
