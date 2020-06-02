module Asmb.IL.Optimization

open Asmb
open Asmb.IL

type optimizing = lines -> lines option
type 'state Optimizer = Optimizer of inputMinLength: int * initialState: 'state * optimizing: ('state ref -> optimizing)

module private PartialPatterns =
    let (|Mov|_|) = function Inst ("mov", [oper1; oper2]) -> Some (oper1, oper2) | _ -> None
    let (|MovReg|_|) = function Mov (Reg r, oper2) -> Some (r, oper2) | _ -> None
    let (|Push|_|) = function Inst ("push", [oper]) -> Some oper | _ -> None
    let (|PushReg|_|) = function Push (Reg r) -> Some r | _ -> None
    let (|Pop|_|) = function Inst ("pop", [oper]) -> Some oper | _ -> None
    let (|PopReg|_|) = function Pop (Reg r) -> Some r | _ -> None

    let (|RegSetter|_|) = function
        | Pop (Reg r) -> Some(r, fun x -> Line.make "pop" [Reg x])
        | MovReg (r, o) -> Some(r, fun x -> Line.mov x o)
        | _ -> None

    let (|MayModify|_|) r = function
        | Mov (Reg r, _) 
        | Inst ("add", [Reg r; _]) 
        | Inst ("sub", [Reg r; _]) -> 
            match r with 
            | Register.Contains r -> Some()
            | _ -> None
        | Push _ -> None
        | Pop (Reg (Register.Contains r)) -> Some ()
        | Pop _ -> None
        | Comment _ -> None
        | EmptyLine -> None
        | _ -> Some ()

    let (|UsesReg|_|) r = function
        | Inst (_, operands) 
          when List.exists (fun o -> Operand.uses r o) operands -> 
            Some()
        | Inst (("add" | "sub" | "mov" | "pop" | "push"), _) 
        | EmptyLine | Comment _ -> None
        | _ -> Some()

module Optimizer =
    let optimize (Optimizer (inputMinLength, state: 'state, optimizing)) lines =
        let mutable anyRanSuccessfuly, lines, outputLines = false, lines, []
        let state = ref state
        while inputMinLength <= List.length lines do
            match optimizing state lines with
            | Some lines' ->
                //lines <- lines'
                lines <- List.tail lines'
                outputLines <- List.head lines' :: outputLines
                anyRanSuccessfuly <- true
            | None ->
                match lines with
                | h :: t -> 
                    lines <- t
                    outputLines <- h :: outputLines
                | [] -> failwith ""
        if anyRanSuccessfuly 
        then Some <| (List.rev outputLines) @ lines
        else None

    let optimizeFlip l o = optimize l o

    module Optimizers =

        open PartialPatterns


        let redundantPushPop = Optimizer (2, (), fun _ -> function
            | Push oper :: Pop oper' :: r
            | Pop oper :: Push oper' :: r
                when oper = oper' -> 
                Some r
            | Push oper1 :: Pop oper2 :: r 
                when Operand.size oper1 = Operand.size oper2 ->
                Some (Inst ("mov", [oper2; oper1]) :: r)
            | _ -> None)

        let combineConstantMovs = Optimizer (2, (), fun _ -> function
            |   MovReg (ABCDReg (r, L), Constent (UInt low))
              ::MovReg (ABCDReg (r', H), Constent (UInt high))
              ::rest
            |   MovReg (ABCDReg (r, H), Constent (UInt high))
              ::MovReg (ABCDReg (r', L), Constent (UInt low))
              ::rest 
              when r = r' -> 
                Some (Inst ("mov", [Reg (ABCDReg (r, X)); Constent <| UInt (low + high * 256u)]) :: rest)
            | _ -> None)

        let redundantMov0 r = Optimizer(1, false, fun isZero -> function 
            | MovReg (Register.Contains r as r', Constent (UInt 0u)) :: rest ->
                let ret = if isZero.Value && r = r' then Some rest else None
                isZero := true
                ret
            | MayModify r :: _ ->
                isZero := false
                None
            | _ -> None)

        let removeRegisterHopping = Optimizer(3, (), fun _ -> function 
            |   RegSetter (a, firstSetter) 
              ::MovReg (b, Reg a')
              ::RegSetter (a'', secondSetter)
              :: r 
              when a = a' && a = a''-> 
                Some (firstSetter b :: secondSetter a :: r)
            | _ -> None)

        //  TODO: This optimizer can destroy the stack
        let removeUnusedMovs = Optimizer(2, (), fun _ -> function
            | MovReg (a, _) :: r 
              when Seq.contains a Register.Registers.basicRegisters ->
                //  Some true => A setter was found before the register was used
                //  Some false => The register was used before the setter was found
                //  None => A setter was not found
                match List.tryPick (function 
                    | RegSetter (a', _) when a = a' -> Some true
                    | UsesReg a -> Some false
                    | _ -> None) r 
                    with
                | Some true -> Some r
                | Some false | None -> None
            | _ -> None)

        let redundentPushBlockPop = Optimizer(3, (), fun _ -> function
            | PushReg a :: r 
              when Seq.contains a Register.Registers.basicRegisters ->
                let rec inner acc = function
                    | PopReg a' :: r when a = a' -> Some (acc @ r)
                    | (UsesReg a | Pop _) :: _ | [] -> None
                    | l :: r -> inner (acc @ [l]) r
                inner [] r
            | _ -> None)
            

    let rec runOptimizings (optimizings: optimizing seq) lines =
        optimizings 
        |> Seq.fold (fun (ran, lines) optimizing -> 
                        match optimizing lines with
                        | Some lines -> true, lines
                        | None -> ran, lines) (false, lines)
        |> function
        | false, lines -> lines
        | true, lines -> runOptimizings optimizings lines


open Optimizer
open Optimizer.Optimizers

let runAllOptimizers = 
    runOptimizings (List.collect id [
        [
            optimize redundantPushPop
            optimize combineConstantMovs
            optimize removeRegisterHopping
            optimize removeUnusedMovs
            optimize redundentPushBlockPop
        ]
        [for r in Register.Registers.basicRegisters -> optimize (redundantMov0 r)]
    ])

let optimizeProcedure (procedure: Procedure) = {procedure with Body = runAllOptimizers procedure.Body}
let optimizeProgram (program: Program) = {program with Code = program.Code |> List.map optimizeProcedure}
