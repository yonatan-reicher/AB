module Asmb.Translate

open Asmb
open Asmb.AST
open Asmb.IL



/// An Asmb variable is a sequence of lines that loads some data onto the stack
type variables = Map<string, Size * Operand>

type procSigs = Map<string, Size * Size list>

/// This data type is mutable
type Context = 
    {   ///<summary>The variables that exist in the current scope</summary>
        mutable Vars: variables 
        ///<summary>The signitures of all the procedures in the context</summary>
        ProcSigs: procSigs
        ///<summary>The amount of stack bytes taken up since the last ret line</summary>
        mutable StackAllocSize: int
        mutable LocalLabels: string Set
        Rand: System.Random
        Procedure: AsmbProcedure    }
    member t.SizeMap = Map.map (fun _ -> fst) t.Vars, t.ProcSigs
    member t.GenerateLocal(comment: string): Label = 
        //let mutable name = (sprintf "%s__%s" t.Procedure.ProcName comment).Replace(' ', '_')
        //while Set.contains name t.LocalLables do
        //    name <- name + string (Seq.countBy (fun (s: string) -> s.StartsWith name) t.LocalLables)
        //Local name, { t with LocalLables = t.LocalLables.Add name}
        let mutable label = String.map (fun c -> if System.Char.IsLetterOrDigit c then c else '_') (sprintf "%s__%s" t.Procedure.ProcName comment)
        while Set.contains label t.LocalLabels do
            label <- label + string(t.Rand.Next(0, 10))
        t.LocalLabels <- Set.add label t.LocalLabels
        Local label

module Context =
    let ofProgram { ProgVariables = vars; ProgProcedures = procs } =
        let variableOper name size: Operand = Reg <| Var (name, size)
        {   Vars = List.map (fun (name, size, _) -> name, (size, variableOper name size)) vars |> Map.ofList
            ProcSigs = List.map (fun {ProcName = n; RetSize = s; Parameters = p} -> n, (s,List.map snd p)) procs |> Map.ofList 
            StackAllocSize = 0
            Procedure = List.head procs
            LocalLabels = Set.empty
            Rand = new System.Random() }
    
    let declare (name, size) con: unit =
        let oper = Index (BP, UInt <| uint32 con.StackAllocSize, size)
        con.StackAllocSize <- con.StackAllocSize + Size.bytes size
        con.Vars <- Map.add name (size, oper) con.Vars

    let private handleVar continuation name (con: Context) = 
        let size, oper = con.Vars.[name]
        let r = Register.fromSize A size
        continuation r oper

    let pushVar name con= handleVar (fun r oper -> Line.mov r oper :: Line.push r) name con
    let popVar name con= handleVar (fun r oper -> Line.pop r @ [Inst("mov", [oper; Reg r])]) name con


let rec translateAssignTo ({ Vars = v } as con: Context) (expr: Expr): lines = 
    match expr with
    | Expr.Variable name -> 
        Context.popVar name con
    | UOperation (PointerVal, pointer) ->
        if Expr.size con.SizeMap pointer <> Word then failwithf "must be word"
        let r = Register.fromSize A Word
        translateExpr con pointer
        @ [Line.make "pop" [Reg DI]]
        @ Line.pop r
        @ [Line.make "mov" [Index (DI, UInt 0u, DWord); Reg r]]
    | _ -> invalidArg "expr" ("Can only assign to variables. Was a " + string expr)

and translateExpr con expr = 
    match expr with
    | Expr.Constent l -> let r = Register.fromSize A (Expr.size con.SizeMap expr) in Line.mov r (Constent l) :: Line.push r
    | Variable name -> Context.pushVar name con
    | BiOperation (Add | Sub as o, e1, e2) ->
        let size = Size.max (Expr.size con.SizeMap e1) (Expr.size con.SizeMap e2)
        let a, d = Register.fromSize A size, Register.fromSize D size
        translateExpr con (Convert (e1, size))
        @ translateExpr con (Convert (e2, size))
        @ Line.pop d @ Line.pop a
        @ [Line.make (match o with Add -> "add" | Sub -> "sub") [Reg a; Reg d]]
        @ Line.push a
    | BiOperation (Mul, e1, e2) ->
        let size = Size.max (Expr.size con.SizeMap e1) (Expr.size con.SizeMap e2)
        let a, d = Register.fromSize A size, Register.fromSize D size
        translateExpr con (Convert (e1, size))
        @ translateExpr con (Convert (e2, size))
        @ Line.pop d @ Line.pop a
        @ [Line.make "mul" [Reg d]]
        @ Line.push a
    | BiOperation (EQ, e1, e2) ->
        let size = Size.max (Expr.size con.SizeMap e1) (Expr.size con.SizeMap e2)
        let a, d = Register.fromSize A size, Register.fromSize D size
        let equal = con.GenerateLocal <| sprintf "%O = %O" e1 e2
        let notEqual = con.GenerateLocal <| sprintf "%O != %O" e1 e2
        translateExpr con (Convert (e1, size))
        @ translateExpr con (Convert (e2, size))
        @ Line.pop d @ Line.pop a
        @ [Line.make "cmp" [Reg a; Reg d]]
        @ [Line.Jump(JNE, notEqual)]
        @ [Line.make "push" [Constent <| UInt 1u]]
        @ [Line.Jump (JMP, equal)]
        @ [Line.Label notEqual]
        @ [Line.make "push" [Constent <| UInt 0u]]
        @ [Line.Label equal]
    | BiOperation _ -> failwith ""
    //| BiOperation (Div, e1, e2) ->  //  Yes dis one
    //    let size = Size.max (Expr.size con.SizeMap e1) (Expr.size con.SizeMap e2)
    //    let a, d = AFromSize size, DFromSize size
    //    translateExpr con (Convert (e1, size))
    //    @ translateExpr con (Convert (e2, size))
    //    @ Line.pop (Reg d) @ Line.pop (Reg a)
    //    @ [Line.make "mul" [Reg d]]
    //    @ Line.push (Reg a)
    | Expr.Call (name, args) ->
        let retSize, argSizes = con.ProcSigs.[name]
        let r = Register.fromSize A retSize
        let correctSizeArgs = List.map2 (fun e s -> Convert(e,s)) args argSizes
        [Line.make "push" [Reg BP]]
        @ List.collect (fun expr -> Line.comment (sprintf "parameter %O" expr) :: translateExpr con expr) (List.rev correctSizeArgs)
        @ [Call name]
        @ Line.pop r
        @ [Line.make "pop" [Reg BP]]
        @ Line.push r
    | Convert (expr, size) ->
        let aRet = Register.fromSize A size
        let aGet = Register.fromSize A (Expr.size con.SizeMap expr)
        translateExpr con expr
        @ 
        if aRet = aGet then
            []
        else
            [Line.mov0 aRet]
            @ Line.pop aGet
            @ Line.push aRet

//  TODO: Check all the context code. I mean   E V E R Y T H I N G

let rec translateStatement (con: Context) (statement: Statement) =
    match statement with
    | Pushpop ([], block) ->
        translateBlock con block
    | Pushpop (o::l, block) ->
        [Line.comment <| sprintf "Pushing %O" o]
        @ translateExpr con o
        @ [Line.IndentIn]
        @ translateStatement con (Pushpop (l, block))
        @ [Line.IndentOut]
        @ [Line.comment <| sprintf "Popping %O" o]
        @ translateAssignTo con o
    | Assign (assign, expr) ->
        translateExpr con expr
        @ translateAssignTo con assign
    | StackDeclare (name, size, None) ->
        Context.declare (name, size) con
        //  Simply put some zeros to mark it on the stack
        let r = Register.fromSize A size
        [Line.mov0 r]
        @ Line.push r
    | Statement.Comment c -> []//[Line.Comment c]
    | IfElse (cond, trueBlock, falseBlock) ->
        let r = Register.fromSize A (Expr.size con.SizeMap cond)
        let trueLines = translateBlock con trueBlock
        let falseLabel, falseLines = translateBlockWithLabel con falseBlock
        translateExpr con cond
        @ Line.pop r @ [Line.make "cmp" [Reg r; Constent <| UInt 0u]; Line.Jump(JE, falseLabel)]
        @ [IndentIn]
        @ trueLines
        @ [IndentOut]
        @ falseLines
    | While _ -> failwith ""
    | Return None -> translateStatement con (Return (Some (Expr.Constent <| UInt 0u)))
    | Return (Some expr) ->
        let a = Register.fromSize A (Expr.size con.SizeMap expr)
        let d = Register.fromSize D Word
        translateExpr con expr
        @ Line.pop a
        @ [Line.make "pop" [Reg d]]
        @ [Line.make "add" [Reg SP; Constent (UInt <| uint32 (con.StackAllocSize - 2))]]
        @ Line.push a
        @ Line.push d
        @ [Line.make "ret" []]
    | NativeAssemblyLines lines -> Seq.map Line.Text lines |> List.ofSeq

and translateBlock con (Block (comment, statements)): lines = 
    List.collect (fun s -> EmptyLine :: Line.comment (string s) :: translateStatement con s) statements

/// The block inside the label is indented
and translateBlockWithLabel (con:Context) (Block (comment, statements) as block): Label * lines = 
    let l = con.GenerateLocal (defaultArg comment "")
    l, [Line.Label l; IndentIn] @ translateBlock con block @ [IndentOut]

let translateProc con ({ ProcName = name; ProcBody = statements; Parameters = param; RetSize = ret } as proc) =
    let con = { con with StackAllocSize = con.StackAllocSize + 2; Procedure = proc }
    //let con = List.fold (fun con nameAndSize -> Context.declare nameAndSize con) con param
    List.iter (fun nameAndSize -> Context.declare nameAndSize con) param
    let body = [Line.mov BP (Reg SP)] @ translateBlock con statements
    { Name = name; Body = body; Sig = ret, List.map snd param }

let translateProg ({ ProgVariables = vars; ProgProcedures = procedures } as program): Program =
    let con = Context.ofProgram program
    { StackSize = 16*16*16; Code = List.map (translateProc con) procedures; Data = vars}

