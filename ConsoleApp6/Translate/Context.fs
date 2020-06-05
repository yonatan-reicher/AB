[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Asmb.Translate.Context

open Asmb
open Asmb.AST
open Asmb.IL

let empty = { Vars=Map.empty; Procs=Map.empty; ProcedureStack=0u; ParamStack=0u; Labels=set []; Random = new System.Random () }
let make procs vars = 
    {   Vars = Map.ofSeq <| List.map (fun (a,b,c) -> a, (b, c)) vars
        Procs = Map.ofSeq procs 
        ProcedureStack = 0u
        ParamStack = 0u
        Labels = set []
        Random = new System.Random() }

let declare (name, size) con = 
    let b = Size.bytes size
    let newProcStack = con.ProcedureStack + b
    { con with 
        ProcedureStack = newProcStack
        Vars = Map.add name (size, Index(BP, UInt newProcStack, false, size)) con.Vars } 

let exprSize (expr: Expr) (con: Context) = 
    Expr.size (Seq.map (fun (a, (b, _)) -> a, b) (Map.toSeq con.Vars), Map.toSeq con.Procs) expr
              
let private handleVar continuation name (con: Context) = 
    let size, oper = con.Vars.[name]
    let r = Register.fromSize A size
    continuation r oper

let pushVar name con = handleVar (fun r oper -> Line.mov r oper :: Line.push r) name con
let popVar name con = handleVar (fun r oper -> Line.pop r @ [Inst("mov", [oper; Reg r])]) name con

let private labelMap map = function Local s -> Local <| map s | Gloabal s -> Gloabal <| map s

let rec private validateLabel (con: Context) (label: Label): Label * Context =
    if Set.contains label con.Labels then
        validateLabel con (labelMap (fun x -> x + con.Random.Next().ToString()) label)
    else
        label, { con with Labels = con.Labels.Add label }

let private labelString (str: string) = String.map (function c when List.contains c (['a'..'z']@['0'..'9']@['A'..'Z']) -> c | _ -> '_') str |> fun x -> x.Trim('_')
          
let makeLabel con (cat: LabelCatagory) (cond: Expr) (comment: string option) = 
    sprintf "%A_%O_%s" cat cond (defaultArg comment "")
    |> labelString
    |> Local 
    |> validateLabel con