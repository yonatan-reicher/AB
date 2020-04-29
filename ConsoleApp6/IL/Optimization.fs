module Asmb.IL.Optimization

open Asmb
open Asmb.IL

type optimizing = lines -> lines option
type 'state Optimizer = Optimizer of inputMinLength: int * initialState: 'state * optimizing: ('state ref -> optimizing)

module private PartialPatterns =
    let (|Mov|_|) = function Inst ("mov", [oper1; oper2]) -> Some (oper1, oper2) | _ -> None
    let (|MovReg|_|) = function Mov (Reg r, oper2) -> Some (r, oper2) | _ -> None
    let (|Push|_|) = function Inst ("push", [oper]) -> Some oper | _ -> None
    let (|Pop|_|) = function Inst ("pop", [oper]) -> Some oper | _ -> None
    let (|AReg|_|) = function ABCDReg (A, s) -> Some s | _ -> None 

module Optimizer =
    let optimize (Optimizer (inputMinLength, state: 'state, optimizing)) lines =
        let mutable anyRanSuccessfuly, lines, outputLines = false, lines, []
        let state = ref state
        while inputMinLength <= List.length lines do
            match optimizing state lines with
            | Some lines' ->
                lines <- lines'
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
            | (Push oper1 :: Pop oper2 :: r) as lines ->
                System.Diagnostics.Debug.WriteLine(sprintf "%O" lines)
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

        let redundantMov0AH = 
            let (|ContainsAH|_|) = function AReg (H | X | EX) -> Some () | _ -> None
            let (|MayModifyAH|_|) = function
                | Jump _ | Call _ | Text _ -> Some ()
                | Inst (inst, operands) when 
                    not (List.contains inst ["push"; "cmp"]) 
                    && List.exists (function Reg ContainsAH -> true | _ -> false) operands 
                    -> Some ()
                | _ -> None
            Optimizer(1, false, fun ahIsZero -> function 
                | Mov (Reg ContainsAH, Constent (UInt 0u)) as line :: rest ->
                    let lines = if !ahIsZero then rest else line :: rest 
                    ahIsZero := true
                    Some lines
                | MayModifyAH as line :: rest ->
                    ahIsZero := false
                    None
                | _ -> None)
    
        let combineMovAXAndPushAX = Optimizer(2, (), fun _ -> function
            | Mov (Reg (AReg X), Constent l) :: Push (Reg (AReg X)) :: r -> 
                Some (Inst("push", [Constent l]) :: r )
            | _ -> None)

        let redundantPushMovPop = Optimizer(3, (), fun _ -> function
            | Push o1 :: Mov (o2, o3) :: Pop o1' :: r 
                when o1 = o1' -> 
                Some (Inst("mov", [o2; o3]) :: r) 
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

let runAllOptimizers lines = 
    runOptimizings [
        optimize redundantPushPop
        optimize combineConstantMovs
        optimize combineMovAXAndPushAX
        optimize redundantMov0AH
        optimize redundantPushMovPop
    ] lines

let optimizeProcedure (procedure: Procedure) = {procedure with Body = runAllOptimizers procedure.Body}
let optimizeProgram (program: Program) = {program with Code = program.Code |> List.map optimizeProcedure}
