namespace Asmb.Translate

open Asmb
open Asmb.AST
open Asmb.IL

open Register.Registers

type LabelCatagory = 
    SkipIf | SkipElse | LoopLabel | EndLabel | TrueLabel | FalseLabel
    override t.ToString() =
        match t with
        | SkipIf -> "Skip if"
        | SkipElse -> "Skip else" 
        | LoopLabel -> "Loop"
        | EndLabel -> "End"
        | TrueLabel -> "True"
        | FalseLabel -> "False"

        
type Context = {    Vars: Map<string, Size * Operand>
                    Procs: Map<string, ProcSig>
                    ProcedureStack: uint
                    ParamStack: uint
                    Labels: Label Set
                    Random: System.Random       }
            
type CompilationError = CompilationError of Procedure * Statement * message: string * fatal: bool

type LineWriter = { Lines: lines; Context: Context; errors: CompilationError list }
