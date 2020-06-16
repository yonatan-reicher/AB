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

let rec writeBiEquationOper oppositeJumpType expr e1 e2 = 
    func {
        let! equalLabel = makeLabel (TrueLabel, expr, None)
        let! notEqualLabel = makeLabel(FalseLabel, expr, None)
        let! size = maxExprSize e1 e2
        let a, d = Register.fromSize A size, Register.fromSize D size
        do! writeExpr (Convert (e1, size))
        do! writeExpr (Convert (e2, size))
        do! append (Line.pop d @ Line.pop a)
        do! append1 (Line.make "cmp" [Reg a; Reg d])
        do! append1 (Line.Jump(oppositeJumpType, notEqualLabel))
        do! append1 (Line.make "push" [Constent <| UInt 1u])
        do! append1 (Line.Jump (JMP, equalLabel))
        do! append1 (Line.Label notEqualLabel)
        do! append1 (Line.make "push" [Constent <| UInt 0u])
        do! append1 (Line.Label equalLabel)
    }

and writeExpr (expr: Expr): LineWriter -> LineWriter = 
    match expr with
    | Expr.Constent l | Convert(Expr.Constent l, _) -> 
        func {
            let! size = exprSize expr
            let r = Register.fromSize A size
            do! append (Line.mov r (Constent l) :: Line.push r)
        }
    | Variable name -> 
        pushVar name (addError <| UndefindedName name)
    | UOperation (UOperator.Not, arg) ->
        func {
            let! size = exprSize arg

            let inReg = Register.fromSize A size
            let outReg = Register.fromSize A Byte
            
            do! writeExpr arg
            do! append (Line.pop inReg)
            do! append1 (Line.make "cmp" [Reg inReg; Constent (UInt 0u)])
            do! append1 (Line.make "sete" [Reg outReg])
            do! append (Line.push outReg)
        }        
    | BiOperation (Add | Sub as o, e1, e2) ->
        func {
            let! size = maxExprSize e1 e2
            let a, d = Register.fromSize A size, Register.fromSize D size
            do! writeExpr (Convert (e1, size))
            do! writeExpr (Convert (e2, size))
            do! append (Line.pop d @ Line.pop a)
            do! append1 (Line.make (match o with Add -> "add" | Sub -> "sub") [Reg a; Reg d])
            do! append (Line.push a)
        }
    | BiOperation (Mul, e1, e2) ->
        func {
            let! size = maxExprSize e1 e2
            let a, d = Register.fromSize A size, Register.fromSize D size
            do! writeExpr (Convert (e1, size))
            do! writeExpr (Convert (e2, size))
            do! append (Line.pop d @ Line.pop a)
            do! append1 (Line.make "mul" [Reg d])
            do! append (Line.push a)
        }
    | BiOperation (Div, e1, e2) ->
        func {
            let! size = maxExprSize e1 e2
            let a, b = Register.fromSize A size, Register.fromSize B size
            do! writeExpr (Convert (e1, size))
            do! writeExpr (Convert (e2, size))
            do! append1 (Line.mov0 <| Register.fromSize D size)
            do! append (Line.pop b @ Line.pop a)
            do! append1 (Line.make "div" [Reg b])
            do! append (Line.push a)
        }
    | BiOperation (Mod, e1, e2) ->
        func {
            let! size = maxExprSize e1 e2
            do! writeExpr (Convert (e1, size))
            do! writeExpr (Convert (e2, size))

            let lhReg = Register.fromSize A size
            let rhReg = Register.fromSize B size
            let resReg = match size with Byte -> ah | _ -> Register.fromSize D size

            do! append1 (Line.mov0 resReg)
            do! append (Line.pop rhReg @ Line.pop lhReg)
            do! append1 (Line.make "div" [Reg rhReg])
            do! append (Line.push resReg)
        }
    | BiOperation (EQ, e1, e2) -> writeBiEquationOper JNE expr e1 e2
    | BiOperation (NEQ, e1, e2) -> writeBiEquationOper JE expr e1 e2
    | BiOperation (Greater, e1, e2) -> writeBiEquationOper JNG expr e1 e2
    | BiOperation (NGreater, e1, e2) -> writeBiEquationOper JG expr e1 e2
    | BiOperation (Lesser, e1, e2) -> writeBiEquationOper JNL expr e1 e2
    | BiOperation (NLesser, e1, e2) -> writeBiEquationOper JL expr e1 e2
    | BiOperation _ -> failwith ""
    | Expr.Call (name, args) ->
        procSig name (addError <| UndefindedName name) (function 
            | Void, argSizes ->
                let correctSizeArgs = Seq.map2 (fun e s -> Convert(e,s)) args argSizes
                append1 (Line.make "push" [Reg BP])
                >> append1 IndentIn
                >> Seq.foldBack (fun expr -> append1 (Line.comment (sprintf "parameter %O" expr)) >> writeExpr expr) correctSizeArgs
                >> append1 (Call name)
                >> append1 IndentOut
                >> append1 (Line.make "pop" [Reg BP])
            | retSize, argSizes -> 
                let r = Register.fromSize A retSize 
                let correctSizeArgs = Seq.map2 (fun e s -> Convert(e,s)) args argSizes
                append1 (Line.make "push" [Reg BP])
                >> append1 IndentIn
                >> Seq.foldBack (fun expr -> append1 (Line.comment (sprintf "parameter %O" expr)) >> writeExpr expr) correctSizeArgs
                >> append1 (Call name)
                >> append (Line.pop r)
                >> append1 IndentOut
                >> append1 (Line.make "pop" [Reg BP])
                >> append (Line.push r))
    | Convert (expr, retSize) ->
        writeExpr expr
        >> exprSize expr (fun getSize -> 
            if retSize = getSize then id 
            else 
                let aRet = Register.fromSize A retSize
                let aGet = Register.fromSize A getSize
                append1 (Line.mov0 aRet)
                >> append (Line.pop aGet)
                >> append (Line.push aRet))

let writeAssignTo (expr: Expr): LineWriter -> LineWriter = 
    match expr with
    | Expr.Variable name -> popVar name (addError <| UndefindedName name)
    | UOperation (PointerVal, pointer) ->
        let validateSize size = if size <> Word then failwithf "must be word" else size

        exprSize pointer (validateSize >> Register.fromSize A >> fun r ->
            writeExpr pointer
            >> append1 (Line.make "pop" [Reg DI])
            >> append (Line.pop r)
            >> append1 (Line.make "mov" [Index (DI, UInt 0u, true, DWord); Reg r]))
    | _ -> invalidArg "expr" (sprintf "Can only assign to variables. Was a %O" expr)