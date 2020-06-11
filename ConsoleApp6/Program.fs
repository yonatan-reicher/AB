open System
open System.Diagnostics
open System.IO

open FParsec
open Asmb
//open Asmb.AST.Parsing
//open Asmb.IL.Optimization
//open Asmb.IL.Write
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

let defaultTasmPath = @"C:\TASM"
let defaultDosboxPath = @"C:\Program Files (x86)\DOSBox-0.74-3\DOSBox.exe"

let textToAsmbProgram sourceCode: Result<_,_> = 
    match run Asmb.AST.Parsing.pprogram sourceCode with
    | Failure (error, _, _) -> Result.Error error
    | Success (asmbProgram, (), _) -> Result.Ok asmbProgram 

/// Compiles asmb code from a given source file from the TASM folder. fileName is the name of the file in the TASM folder (reletive path)
let compile (dosboxEXEPath: string) (tasmPath: string) (libs: AST.TopLevel.AsmbProgram seq) (optimize: bool) (fileName: string) =
    let stringPath (folder, file, fileExtension) = sprintf "%s\%s.%s" folder file fileExtension

    let fileName = if fileName.EndsWith ".ab" then fileName.[0..fileName.Length-1-3] else fileName
    let sourceCode = File.ReadAllText (stringPath(tasmPath, fileName, "ab"))
    
    match textToAsmbProgram sourceCode with
    | Result.Error error -> printfn "%s" error
    | Result.Ok asmbProgram ->
        let program, errors = translateProgram libs asmbProgram 
        if Seq.exists (fst >> function CompilationError.Fatal -> true | _ -> false) errors then
            printfn "Cannot compile the program"
        else 
            let program = if optimize then IL.Optimization.optimizeProgram program else program
            let str = IL.Write.writeProg program     
            printfn "%s" str
            File.WriteAllText(stringPath(tasmPath, fileName, "asm"), str)
        
            use p = System.Diagnostics.Process.Start(dosboxEXEPath, sprintf @"-c ""TASM %s"" -c ""TLINK %s""" fileName fileName)
            Interop.onExit p.Kill
            p.WaitForExit()

        printfn "Errors: "
        for err in errors do printfn "\t%s %A" (match fst err with CompilationError.Fatal -> "ERROR" | _ -> "Warning") (fst err)

[<EntryPoint>]
let rec main args =
    match args with
    | [|"-lib"; filePath |] ->
        let filePath = if filePath.EndsWith ".ab" then filePath.[0..filePath.Length-1-3] else filePath
        let sourcePath = sprintf "%s.ab" filePath
        use destFile = File.Create (sprintf "%s.ablib" filePath)
        printfn "Writing '%s' to '%s'" sourcePath destFile.Name

        File.ReadAllText sourcePath
        |> textToAsmbProgram
        |> Result.map (writeProgramToStream destFile) 
        |> function Result.Error e -> printfn "%O" e | Result.Ok () -> printfn "Done!"
    | [|fileName|] ->
        compile defaultDosboxPath defaultTasmPath (seq { use a = File.OpenRead @"C:\TASM\std.ablib" in readProgramFromStream a }) true fileName
    | [|dosboxPath; tasmPath; fileName|] ->
        compile dosboxPath tasmPath Seq.empty true fileName
    | _ -> printfn "Incorrect arguments! \nUse: \nasmb file-name \nor: \nasmb dosbox-path tasm-folder-path file-name \nor: \nasmb -lib lib-name"
    Console.ReadKey false |> ignore
    0
