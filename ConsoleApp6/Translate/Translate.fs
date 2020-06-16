namespace Asmb.Translate

open Asmb
open Asmb.AST
open Asmb.IL


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
                    Funcs: Map<string, FuncSig>
                    ProcedureStack: uint
                    ParamStack: uint
                    Function: Function
                    Block: Block
                    Labels: Label Set
                    Random: System.Random       }
            
type CompilationError = 
    | UndefindedName of string
    | UnsupportedFeature of obj*string
type Pos = Pos of Block
module CompilationError =
    let (|Fatal|Warning|): CompilationError -> Choice<unit,unit> = function
        | UndefindedName _
        | UnsupportedFeature _ -> Fatal

type LineWriter = { Lines: lines; Context: Context; Errors: (CompilationError * Pos) list }
