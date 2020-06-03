module Asmb.Translate

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

type Context = { Vars: Map<string, Size * Operand>
                 Procs: Map<string, ProcSig>
                 ProcedureStack: uint
                 ParamStack: uint
                 Labels: Label Set
                 Random: System.Random}

module Context =
    let empty = { Vars=Map.empty; Procs=Map.empty; ProcedureStack=0u; ParamStack=0u; Labels=set []; Random = new System.Random () }
    let make procs vars = 
        { Vars = Map.ofSeq <| List.map (fun (a,b,c) -> a, (b, c)) vars
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

type LineWriter = { Lines: lines; Context: Context }
module LineWriter =
    let empty = { Lines=[]; Context=Context.empty}
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

open LineWriter

let jmpFromOper = function  | EQ -> JE | NEQ -> JNE 
                            | Greater -> JG | Lesser -> JL
                            | NGreater -> JNG | NLesser -> JNL

let rec translateBiEquationOper oppositeJumpType expr e1 e2 = 
    makeLabel (TrueLabel, expr, None) (fun equalLabel ->
        makeLabel (FalseLabel, expr, None) (fun notEqualLabel ->
            exprSize e1 (fun size1 -> 
                exprSize e2 (fun size2 ->
                    let size = Size.max size1 size2
                    let a, d = Register.fromSize A size, Register.fromSize D size
                    translateExpr (Convert (e1, size))
                    >> translateExpr (Convert (e2, size))
                    >> append (Line.pop d @ Line.pop a)
                    >> append1 (Line.make "cmp" [Reg a; Reg d])
                    >> append1 (Line.Jump(oppositeJumpType, notEqualLabel))
                    >> append1 (Line.make "push" [Constent <| UInt 1u])
                    >> append1 (Line.Jump (JMP, equalLabel))
                    >> append1 (Line.Label notEqualLabel)
                    >> append1 (Line.make "push" [Constent <| UInt 0u])
                    >> append1 (Line.Label equalLabel)))))

and translateExpr (expr: Expr): LineWriter -> LineWriter = 
    match expr with
    | Expr.Constent l | Convert(Expr.Constent l, _) -> 
        exprSize expr (Register.fromSize A >> fun r -> 
            append (Line.mov r (Constent l) :: Line.push r))
    | Variable name -> pushVar name
    | BiOperation (Add | Sub as o, e1, e2) ->
        exprSize e1 (fun size1 -> 
            exprSize e2 (fun size2 ->
                let size = Size.max size1 size2
                let a, d = Register.fromSize A size, Register.fromSize D size
                translateExpr (Convert (e1, size))
                >> translateExpr (Convert (e2, size))
                >> append (Line.pop d @ Line.pop a)
                >> append1 (Line.make (match o with Add -> "add" | Sub -> "sub") [Reg a; Reg d])
                >> append (Line.push a)
                ))
    | BiOperation (Mul, e1, e2) ->
        exprSize e1 (fun size1 -> 
            exprSize e2 (fun size2 ->
                let size = Size.max size1 size2
                let a, d = Register.fromSize A size, Register.fromSize D size
                translateExpr (Convert (e1, size))
                >> translateExpr (Convert (e2, size))
                >> append (Line.pop d @ Line.pop a)
                >> append1 (Line.make "mul" [Reg d])
                >> append (Line.push a)
                ))
    | BiOperation (Div, e1, e2) ->
        exprSize e1 (fun size1 -> 
            exprSize e2 (fun size2 ->
                let size = Size.max size1 size2
                let a, b = Register.fromSize A size, Register.fromSize B size
                translateExpr (Convert (e1, size))
                >> translateExpr (Convert (e2, size))
                >> append1 (Line.mov0 <| Register.fromSize D size)
                >> append (Line.pop b @ Line.pop a)
                >> append1 (Line.make "div" [Reg b])
                >> append (Line.push a)))
    | BiOperation (Mod, e1, e2) ->
        exprSize expr (function 
            | Void -> id
            | Byte -> 
                translateExpr (Convert (e1, Byte))
                >> translateExpr (Convert (e2, Byte))
                >> append (Line.pop bl @ Line.pop al)
                >> append1 (Line.make "div" [Reg bl])
                >> append (Line.push ah)
            | Word -> 
                translateExpr (Convert (e1, Word))
                >> translateExpr (Convert (e2, Word))
                >> append1 (Line.mov0 dx)
                >> append (Line.pop bx @ Line.pop ax)
                >> append1 (Line.make "div" [Reg bx])
                >> append (Line.push dx)
            | DWord -> 
                translateExpr (Convert (e1, DWord))
                >> translateExpr (Convert (e2, DWord))
                >> append1 (Line.mov0 edx)
                >> append (Line.pop ebx @ Line.pop eax)
                >> append1 (Line.make "div" [Reg ebx])
                >> append (Line.push edx))
    | BiOperation (EQ, e1, e2) -> translateBiEquationOper JNE expr e1 e2
    | BiOperation (NEQ, e1, e2) -> translateBiEquationOper JE expr e1 e2
    | BiOperation (Greater, e1, e2) -> translateBiEquationOper JNG expr e1 e2
    | BiOperation (NGreater, e1, e2) -> translateBiEquationOper JG expr e1 e2
    | BiOperation (Lesser, e1, e2) -> translateBiEquationOper JNL expr e1 e2
    | BiOperation (NLesser, e1, e2) -> translateBiEquationOper JL expr e1 e2
    | BiOperation _ -> failwith ""
    | Expr.Call (name, args) ->
        procSig name (function 
            | Void, argSizes ->
                let correctSizeArgs = Seq.map2 (fun e s -> Convert(e,s)) args argSizes
                append1 (Line.make "push" [Reg BP])
                >> append1 IndentIn
                >> Seq.foldBack (fun expr -> append1 (Line.comment (sprintf "parameter %O" expr)) >> translateExpr expr) correctSizeArgs
                >> append1 (Call name)
                >> append1 IndentOut
                >> append1 (Line.make "pop" [Reg BP])
            | retSize, argSizes -> 
                let r = Register.fromSize A retSize 
                let correctSizeArgs = Seq.map2 (fun e s -> Convert(e,s)) args argSizes
                append1 (Line.make "push" [Reg BP])
                >> append1 IndentIn
                >> Seq.foldBack (fun expr -> append1 (Line.comment (sprintf "parameter %O" expr)) >> translateExpr expr) correctSizeArgs
                >> append1 (Call name)
                >> append (Line.pop r)
                >> append1 IndentOut
                >> append1 (Line.make "pop" [Reg BP])
                >> append (Line.push r))
    | Convert (expr, retSize) ->
        translateExpr expr
        >> exprSize expr (fun getSize -> 
            if retSize = getSize then id 
            else 
                let aRet = Register.fromSize A retSize
                let aGet = Register.fromSize A getSize
                append1 (Line.mov0 aRet)
                >> append (Line.pop aGet)
                >> append (Line.push aRet))

let translateAssignTo (expr: Expr): LineWriter -> LineWriter = 
    match expr with
    | Expr.Variable name -> popVar name
    | UOperation (PointerVal, pointer) ->
        let validateSize size = if size <> Word then failwithf "must be word" else size

        exprSize pointer (validateSize >> Register.fromSize A >> fun r ->
            translateExpr pointer
            >> append1 (Line.make "pop" [Reg DI])
            >> append (Line.pop r)
            >> append1 (Line.make "mov" [Index (DI, UInt 0u, true, DWord); Reg r]))
    | _ -> invalidArg "expr" (sprintf "Can only assign to variables. Was a %O" expr)

let translateCondition falseLabel cond = 
        match cond with
        | BiOperation(BiOperator.Equation as o, e1, e2) -> 
            exprSize e1 (fun size1 -> 
                exprSize e2 (fun size2 -> 
                    let size = Size.max size1 size2
                    let r1, r2 = Register.fromSize A size, Register.fromSize D size
                    translateExpr (Convert(e1, size))
                    >> translateExpr (Convert(e2, size))
                    >> append (Line.pop r2)
                    >> append (Line.pop r1)
                    >> append1 (Line.make "cmp" [Reg r1; Reg r2])))
            >> append1 (Jump(JumpType.not (jmpFromOper o), falseLabel)) 
        | _ ->
            translateExpr cond
            >> exprSize cond (Register.fromSize A >> fun r -> 
                append (Line.pop r)
                >> append1 (Line.make "cmp" [Reg r; Constent <| UInt 0u]))
            >> append1 (Line.Jump (JE, falseLabel))

let rec translateStatement (statement: Statement): LineWriter -> LineWriter = 
    match statement with
    | Pushpop ([], block) -> 
        translateBlock block
    | Pushpop (o::l, block) ->
        append1 (Line.comment <| sprintf "Pushing %O" o)
        >> translateExpr o
        >> append1 IndentIn
        >> translateStatement (Pushpop (l, block))
        >> append1 IndentOut
        >> append1 (Line.comment <| sprintf "Popping %O" o)
        >> translateAssignTo o
    | SideEffect e ->
        exprSize e (fun size -> 
            translateExpr e >> append1 (Line.make "add" [Reg SP; Size.bytes size |> UInt |> Constent]))
    | Assign (assign, expr) ->
        exprSize assign (fun size ->
            translateExpr (Convert(expr, size)) >> translateAssignTo assign)
    | StackDeclare (name, size, None) ->
        let r = Register.fromSize A size
        //  Simply put some zeros to mark it on the stack
        declare (name, size) 
        >> append1 (Line.mov0 r)
        >> append (Line.push r)
    | StackDeclare (name, size, Some expr) ->
        declare (name, size) >> translateExpr (Convert (expr, size))
    | Statement.Comment c -> id //[Line.Comment c]
    | IfElse (cond, trueBlock, falseBlock) ->
        let appendIfBody skipElse =
            makeLabel (SkipIf, cond, falseBlock |> Option.bind (fun b -> b.Comment)) (fun skipIf ->
                translateCondition skipIf cond
                >> append1 IndentIn
                >> translateBlock trueBlock
                >> match skipElse with Some skipElse -> append1 (Jump (JMP, skipElse)) | None -> id
                >> append1 IndentOut
                >> append1 (Line.Label skipIf))

        match falseBlock with
        | None -> 
            appendIfBody None
        | Some falseBlock -> 
            makeLabel(SkipElse, cond, falseBlock.Comment) (fun skipElse ->
                appendIfBody (Some skipElse)
                >> translateBlock falseBlock
                >> append1 (Line.Label skipElse)) 
    | While (cond, block) ->
        makeLabel (EndLabel, cond, None) (fun skip ->
            makeLabel (LoopLabel, cond, block.Comment) (fun loop ->
                append1 (Line.Label loop)
                >> translateCondition skip cond
                >> append1 IndentIn
                >> translateBlock block
                >> append1 (Jump (JMP, loop)))
            >> append1 IndentOut
            >> append1 (Line.Label skip))
    | Return None -> //translateStatement (Return (Some (Expr.Constent <| UInt 0u)))
        procedureStack (fun stack -> append1 (Line.make "add" [Reg SP; Constent (UInt stack)]))
        >> paramStack (fun stack -> append1 (Line.make "ret" [Constent (UInt stack)]))
    | Return (Some expr) ->
        exprSize expr (Register.fromSize A >> fun a -> 
            let d = Register.fromSize D Word
            translateExpr expr
            >> append (Line.pop a)
            >> procedureStack (fun stack -> append1 (Line.make "add" [Reg SP; Constent (UInt stack)]))
            >> append1 (Line.make "pop" [Reg d])
            >> paramStack (fun stack -> append1 (Line.make "add" [Reg SP; Constent (UInt stack)]))
            >> append (Line.push a)
            >> append (Line.push d)
            >> append1 (Line.make "ret" []))
    | UnsafePush e -> translateExpr e
    | UnsafePop e -> translateAssignTo e
    | NativeAssemblyLines lines -> Seq.map Line.Text lines |> List.ofSeq |> append
 

and translateBlock (Block (_, statements)) writer: LineWriter = 
    List.fold (fun writer statement -> 
        writer 
        |> append1 EmptyLine 
        |> match statement with 
            | NativeAssemblyLines _ -> id 
            | _ -> append1 (Line.comment <| string statement) 
        |> translateStatement statement) writer statements

let translateProc (con: Context) (procedure: AsmbProcedure): Procedure =
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
    { Name = procedure.ProcName
      Sig = procedure.Sig
      Body = Line.mov BP (Reg SP) :: (translateBlock procedure.ProcBody <| LineWriter.ofContext con).Lines }

let translateProgram (program: AsmbProgram): Program =
    let con = 
        Context.make 
        <| List.map (fun x -> x.ProcName, x.Sig) program.ProgProcedures 
        <| List.map (fun (name,size,_) -> name, size, Reg (Var (name, size))) program.ProgVariables
    { StackSize = 16*16*16; Data = program.ProgVariables; Code = List.map (translateProc con) program.ProgProcedures }
