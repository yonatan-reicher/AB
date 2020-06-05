module Asmb.Translate.Main

open Asmb
open Asmb.AST
open Asmb.IL

open WriteStatement

let translateProc (con: Context) (procedure: Function): Procedure =
    let con = 
        { con with 
            ParamStack = procedure.Sig |> snd |> Seq.sumBy Size.bytes
            Vars = 
                List.fold 
                    (fun (vars: Map<_,_>, offset) (name, size) -> 
                        Map.add name (size, Index (BP, UInt offset, true, size)) vars, 
                        offset + Size.bytes size) 
                    (con.Vars, 2u)
                    procedure.Parameters //   2u is the number of bytes stored as the line pointer
                |> fst }
    { Name = procedure.Name
      Sig = procedure.Sig
      Body = Line.mov BP (Reg SP) :: (translateBlock procedure.Body <| LineWriter.ofContext con).Lines }


let translateProgram (program: AsmbProgram): Program =
    match program.ProgFunctions |> List.tryFind (fun x -> x.Name = "main" && x.Sig = (Byte, [])) with 
    | None ->
        failwithf "The program needs a 'main' function defined like this: \nfunc byte main() { ... }"
    | Some main -> 
        let funcs = 
            let allFuncs = program.ProgFunctions |> Seq.map (fun x -> x.Name, x) |> Map.ofSeq
            let mutable funcs, nextFuncs = [], [main]
            while not <| List.isEmpty nextFuncs do
                funcs <- Seq.append nextFuncs funcs |> Seq.distinct |> List.ofSeq
                nextFuncs <- 
                    List.ofSeq (Seq.collect (Function.body >> Statement.blockFunctions) nextFuncs 
                                |> Seq.map (fun name -> allFuncs.[name])
                                |> Seq.filter(fun f -> not (List.contains f funcs))) 
            funcs 

        let con = 
            Context.make 
            <| Seq.map (function Function x -> x.Name, x.Sig) funcs
            <| List.map (fun (name,size,_) -> name, size, Reg (Var (name, size))) program.ProgVariables
        { StackSize = 16*16*16; Data = program.ProgVariables; Code = [for f in funcs -> translateProc con f] }