[<AutoOpen>]
module Asmb.Translate.Main

open Asmb
open Asmb.AST
open Asmb.IL

open WriteStatement

let translateFunction (con: Context) (func: Function): Procedure * _ list =
    let con = 
        { con with 
            ParamStack = func.Sig |> snd |> Seq.sumBy Size.bytes
            Vars = 
                List.fold 
                    (fun (vars: Map<_,_>, offset) (name, size) -> 
                        Map.add name (size, Index (BP, UInt offset, true, size)) vars, 
                        offset + Size.bytes size) 
                    (con.Vars, 2u)
                    func.Parameters //   2u is the number of bytes stored as the line pointer
                |> fst }
    let writer = 
        LineWriter.ofContext con
        |> writeBlock func.Body
    let proc = 
        { Name = func.Name
          Sig = func.Sig
          Body = Line.mov BP (Reg SP) :: writer.Lines }
    proc, writer.Errors


let translateProgram libs (program: AsmbProgram): Program * _ list =
    match program.ProgFunctions |> List.tryFind (fun x -> x.Name = "main" && x.Sig = (Byte, [])) with 
    | None ->
        failwithf "The program needs a 'main' function defined like this: \nfunc byte main() { ... }"
    | Some main -> 
        let funcs = 
            let allFuncs = 
                Seq.append libs [program] 
                |> Seq.map (fun x -> x.ProgFunctions) 
                |> Seq.collect id 
                |> Seq.map (fun x -> x.Name, x) 
                |> Map.ofSeq
            let mutable funcs, nextFuncs = [], [main]
            while not <| List.isEmpty nextFuncs do
                funcs <- Seq.append nextFuncs funcs |> Seq.distinct |> List.ofSeq
                nextFuncs <- 
                    List.ofSeq (Seq.collect (Function.body >> Statement.blockFunctions) nextFuncs 
                                |> Seq.map (fun name -> allFuncs.[name])
                                |> Seq.filter(fun f -> not (List.contains f funcs))) 
            funcs 

        let conMaker = //   The longer the icon of sin is on earth, the stronger he becomes
            Context.make 
            <| Seq.map (function Function x -> x.Name, x.Sig) funcs
            <| List.map (fun (name,size,_) -> name, size, Reg (Var (name, size))) program.ProgVariables
        let code, errors = List.unzip [for f in funcs -> translateFunction (conMaker f) f]
        let errors = List.collect id errors
        { StackSize = 16*16*16; Data = program.ProgVariables; Code = code }, errors


open System.Runtime.Serialization.Formatters.Binary

let writeProgramToStream (stream: (*FileStream*)_) (program: AsmbProgram) =
    let b = BinaryFormatter()
    b.Serialize(stream, program)
    ()

let readProgramFromStream (stream): AsmbProgram =
    let b = BinaryFormatter()
    b.Deserialize stream :?> AsmbProgram
