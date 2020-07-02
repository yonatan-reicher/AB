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



let rec writeExpr (expr: Expr): LineWriter -> LineWriter = 
    match expr with
    | Expr.Constent l | Convert(Expr.Constent l, _) -> 
        func {
            let! size = exprSize expr
            let r = Register.fromSize A size
            do! append (Line.mov r (Constent l) :: Line.push r)
        }
    | Variable name -> 
        pushVar name (addError <| UndefindedName name)
    | UOperation (Not, arg) ->
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
    | Convert (UOperation (PointerVal, arg), size) ->
        func {
            let r = Register.fromSize A size
            do! writeExpr (Convert (arg, Word))
            do! append (Line.pop bx)
            do! append1 (Line.mov r (Index (bx, UInt 0u, true, size)))
            do! append (Line.push r)
        }
    | UOperation (PointerVal, _) -> writeExpr (Convert (expr, DWord))
    | UOperation (PointerOf, arg) ->
        match arg with
        | Variable name ->
            let a = Register.fromSize A Word
            leaVar name (Reg a) (addError <| UndefindedName name) >> append (Line.push a)
        | _ -> addError (UnsupportedFeature (arg, "Cannot take address of this expression"))
    | Conditional (cond, e1, e2) -> 
        func {
            let! size = maxExprSize e1 e2
            let! falseLabel = makeLabel (FalseLabel, cond, None) 
            let! trueLabel = makeLabel (TrueLabel, cond, None) 

            do! writeCondition falseLabel cond
            do! writeExpr (Convert(e1, size))
            do! append1 (Jump(JMP, trueLabel))
            do! append1 (Label falseLabel)
            do! writeExpr (Convert(e2, size))
            do! append1 (Label trueLabel)
        }
    | BiOperation (o, e1, e2) -> writeBiOperation o e1 e2
    | Expr.Call (name, args) ->
        func { 
            let! (retSize, argSizes) = procSig name (addError <| UndefindedName name)
            let correctSizeArgs = Seq.map2 (fun e s -> Convert(e,s)) args argSizes
            do! append1 (Line.make "push" [Reg BP])
            do! func {
                do! indented
                for expr in Seq.rev correctSizeArgs do
                    do! append1 (Line.comment (sprintf "parameter %O" expr))
                    do! writeExpr expr
                do! append1 (Call name)
            }
            do! append1 (Line.make "pop" [Reg BP])
            match retSize with 
            | Void -> () 
            | _ -> 
                let resReg = Register.fromSize A retSize
                do! append (Line.push resReg)
        }
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


and writeCondition (falseLabel: Label) (cond: Expr): LineWriter -> LineWriter = 
    match cond with
    | BiOperation(BiOperator.Equation as o, e1, e2) -> 
        func {
            let! size = maxExprSize e1 e2
            let r1, r2 = Register.fromSize A size, Register.fromSize D size
            let jump = Jump (JumpType.not (jmpFromOper o), falseLabel)

            do! writeExpr (Convert(e1, size))
            do! writeExpr (Convert(e2, size))
            do! append (Line.pop r2)
            do! append (Line.pop r1)
            do! append1 (Line.make "cmp" [Reg r1; Reg r2])
            do! append1 jump 
        }
    | _ ->
        func {
            let! size = exprSize cond
            let r = Register.fromSize A size
            do! writeExpr cond
            do! append (Line.pop r)
            do! append1 (Line.make "cmp" [Reg r; Constent <| UInt 0u])
            do! append1 (Line.Jump (JE, falseLabel))
        }


and private writeBiOperation (oper: BiOperator) (e1: Expr) (e2: Expr) =
    func {
        let! size = maxExprSize e1 e2
        
        do! match oper, e1, e2 with
            | NEQ, e, Expr.Constent(UInt 0u) | NEQ, Expr.Constent(UInt 0u), e when size = Byte -> writeExpr e 
            | BiOperator.Logical, e1, e2 ->
                let e2 = BiOperation(NEQ, e2, Expr.Constent <| UInt 0u) //  Safly convert to byte
                match oper with 
                | And -> Conditional (e1, e2, Expr.Constent <| UInt 0u)
                | Or  -> Conditional (e1, Expr.Constent <| UInt 1u, e2)
                | _ -> failwithf "Cannot translate %A operator" oper
                |> writeExpr
            | BiOperator.Arithmetic, e1, e2 ->
                func {
                    let! size = maxExprSize e1 e2                                                       
                    do! writeExpr (Convert (e1, size))
                    do! writeExpr (Convert (e2, size))
                    match oper with
                    | Add | Sub ->
                        let a, d = Register.fromSize A size, Register.fromSize D size
                        do! append (Line.pop d @ Line.pop a)
                        do! append1 (Line.make (match oper with Add -> "add" | Sub -> "sub" | _ -> failwithf "") [Reg a; Reg d])
                        do! append (Line.push a)
                    | Mul ->
                        let a, d = Register.fromSize A size, Register.fromSize D size
                        do! append (Line.pop d @ Line.pop a)
                        do! append1 (Line.make "mul" [Reg d])
                        do! append (Line.push a)
                    | Div ->
                        let a, b = Register.fromSize A size, Register.fromSize B size
                        do! append1 (Line.mov0 <| Register.fromSize D size)
                        do! append (Line.pop b @ Line.pop a)
                        do! append1 (Line.make "div" [Reg b])
                        do! append (Line.push a)
                    | Mod ->
                        let lhReg, rhReg = Register.fromSize A size, Register.fromSize B size
                        let resReg = match size with Byte -> ah | _ -> Register.fromSize D size
                        do! append1 (Line.mov0 resReg)
                        do! append (Line.pop rhReg @ Line.pop lhReg)
                        do! append1 (Line.make "div" [Reg rhReg])
                        do! append (Line.push resReg)
                    | _ -> do! addError (CompilationError.UnsupportedFeature (oper, "Not an arithmetic operator"))
                }
            | BiOperator.Equation, e1, e2 ->
                func {
                    let! size = maxExprSize e1 e2
                    let a, d = Register.fromSize A size, Register.fromSize D size
                    let resReg = Register.fromSize A Byte
                    let setInst = 
                        match oper with 
                        //  These are apperantly signed operations o_O
                        //| EQ -> "sete" | NEQ -> "setne"
                        //| Greater -> "setg" | Lesser -> "setl"
                        //| NGreater -> "setng" | NLesser -> "setnl"
                        | EQ -> "sete" | NEQ -> "setne"
                        | Greater -> "seta" | Lesser -> "setb"
                        | NGreater -> "setna" | NLesser -> "setnb"
                        | _ -> failwithf "Cannot translate %A operator" oper

                    do! writeExpr e1
                    do! writeExpr e2
                    do! append (Line.pop d @ Line.pop a)
                    do! append1 (Line.make "cmp" [Reg a; Reg d])
                    do! append1 (Line.make setInst [Reg resReg])
                    do! append (Line.push resReg)
                }
    }


let rec writeAssignTo (expr: Expr): LineWriter -> LineWriter = 
    match expr with
    | Expr.Variable name -> popVar name (addError <| UndefindedName name)
    | Convert (UOperation (PointerVal, pointer), size) ->
        func {
            let r = Register.fromSize A size
            do! writeExpr (Convert (pointer, Word))
            do! append1 (Line.make "pop" [Reg DI]) 
            do! append (Line.pop r)
            do! append1 (Line.make "mov" [Index(DI, UInt 0u, true, size); Reg r])
        }
    | UOperation (PointerVal, _) -> 
        func {
            let! size = exprSize expr
            do! writeAssignTo (Convert (expr, size))
        }
    | _ -> invalidArg "expr" (sprintf "Can only assign to variables. Was a %O" expr)