[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Asmb.Translate.LineWriter

open Asmb.Translate
open Asmb
open Asmb.AST
open Asmb.IL

//let empty = { Lines=[]; Context=Context.empty; Errors = [] }
let ofContext context = { Lines=[]; Context = context; Errors = [] }

let append lines writer = { writer with Lines = writer.Lines @ lines }
let append1 line = append [line]

let indented continuation = append1 IndentIn >> continuation >> append1 IndentOut

let getFunc cont writer: LineWriter = cont writer.Context.Function writer
let getBlock cont writer: LineWriter = cont writer.Context.Block writer

let declare (name, size) writer = { writer with Context = Context.declare (name, size) writer.Context }

let addError error (writer: LineWriter) = { writer with Errors = (error, Pos writer.Context.Block) :: writer.Errors }

let exprSize expr continuation (writer: LineWriter): LineWriter = 
    match Context.exprSize expr writer.Context with
    | Error e -> addError e
    | Ok size -> continuation size
    <| writer
let maxExprSize e1 e2 (cont: Size->LineWriter->LineWriter) = 
    func { 
        let! s1 = exprSize e1 
        let! s2 = exprSize e2 
        do! cont (Size.max s1 s2)
    }

let makeLabel (cat, cond, comment) continuation writer = 
    let label, con = Context.makeLabel writer.Context cat cond comment
    continuation label { writer with Context = con }

let pushVar name error writer = 
    match Context.pushVar name writer.Context with 
    | None -> error
    | Some lines -> append lines
    <| writer
let popVar name error writer = 
    match Context.popVar name writer.Context with 
    | None -> error
    | Some lines -> append lines
    <| writer

let procSig name error continuation (writer: LineWriter): LineWriter =
    match Context.funcSig name writer.Context with 
    | None -> error
    | Some funcSig -> continuation funcSig
    <| writer

let procedureStack continuation (writer: LineWriter) = continuation <| writer.Context.ProcedureStack <| writer
let paramStack continuation writer = continuation <| writer.Context.ParamStack <| writer
