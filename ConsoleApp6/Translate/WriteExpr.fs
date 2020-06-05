module Asmb.Translate.WriteExpr

open Asmb
open Asmb.AST
open Asmb.IL

open LineWriter
open Register.Registers

let jmpFromOper = function  | EQ -> JE | NEQ -> JNE 
                            | Greater -> JG | Lesser -> JL
                            | NGreater -> JNG | NLesser -> JNL
                            | x -> invalidArg "_arg1" (sprintf "Cannot use jmpFromOper on %O!" x)

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