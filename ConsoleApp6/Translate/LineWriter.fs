[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Asmb.Translate.LineWriter

open Asmb.Translate
open Asmb
open Asmb.AST
open Asmb.IL

let empty = { Lines=[]; Context=Context.empty; errors = [] }
let ofContext context = { empty with Context = context }

let append lines writer = { writer with Lines = writer.Lines @ lines }
let append1 line = append [line]

let indented continuation = append1 IndentIn >> continuation >> append1 IndentOut

let declare (name, size) writer = { writer with Context = Context.declare (name, size) writer.Context }

let exprSize expr continuation (writer: LineWriter): LineWriter = continuation <| Context.exprSize expr writer.Context <| writer

let makeLabel (cat, cond, comment) continuation writer = 
    let label, con = Context.makeLabel writer.Context cat cond comment
    continuation label { writer with Context = con }

let pushVar name writer = append (Context.pushVar name writer.Context) writer
let popVar name writer = append (Context.popVar name writer.Context) writer

let procSig name continuation (writer: LineWriter) = continuation <| writer.Context.Procs.[name] <| writer

let procedureStack continuation (writer: LineWriter) = continuation <| writer.Context.ProcedureStack <| writer
let paramStack continuation writer = continuation <| writer.Context.ParamStack <| writer