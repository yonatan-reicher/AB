[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Asmb.Translate.Context

open Asmb
open Asmb.AST
open Asmb.IL

//let empty = 
//    {   Vars=Map.empty
//        Funcs=Map.empty
//        ProcedureStack=0u
//        ParamStack=0u
//        Function=
//        Labels=set []
//        Random=new System.Random () }
let make funcs vars func = 
    {   Vars = Map.ofSeq <| List.map (fun (a,b,c) -> a, (b, c)) vars
        Funcs = Map.ofSeq funcs 
        ProcedureStack = 0u
        ParamStack = 0u
        Function = func
        Block = func.Body
        Labels = set []
        Random = new System.Random() }

let declare (name, size) con = 
    let b = Size.bytes size
    let newProcStack = con.ProcedureStack + b
    { con with 
        ProcedureStack = newProcStack
        Vars = Map.add name (size, Index(BP, UInt newProcStack, false, size)) con.Vars } 

let exprSize (expr: Expr) (con: Context) = 
    let getter map name = 
        match map name with
        | Some a -> Ok a
        | None -> Error (UndefindedName name)
    Expr.size (getter (con.Vars.TryFind>>Option.map fst), 
               getter (con.Funcs.TryFind>>Option.map fst)) expr
              
let private getVar continuation name (con: Context): lines option = 
    con.Vars.TryFind name |> Option.map (fun (size, oper) -> Register.fromSize A size, oper) |> continuation
 
let pushVar name con = getVar (Option.map (fun (r, oper) -> Line.mov r oper :: Line.push r)) name con
let popVar name con = getVar (Option.map (fun (r, oper) -> Line.pop r @ [Inst("mov", [oper; Reg r])])) name con
let leaVar name oper con = getVar (Option.map (fun (_, varOper) -> [Line.make "lea" [oper; varOper]])) name con

let funcSig name (con: Context) = con.Funcs.TryFind name

let private labelMap map = function Local s -> Local <| map s | Global s -> Global <| map s

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