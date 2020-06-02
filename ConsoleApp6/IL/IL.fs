[<AutoOpen>]
module Asmb.IL.TopLevel

open Asmb

type JumpType = 
    | JMP
    | JG    | JL    | JE
    | JNG   | JNL   | JNE
    | JGE   | JLE
    override t.ToString() =
        match t with
        | JMP -> "jmp"
        | JG -> "jg"
        | JL -> "jl"
        | JE -> "je"
        | JNG -> "jng"
        | JNL -> "jnl"
        | JNE -> "jne"
        | JGE -> "jge"
        | JLE -> "jle"
module JumpType =
    let not = function
        | JG  -> JNG | JL  -> JNL  | JE -> JNE
        | JNG -> JG  | JNL -> JL   | JNE -> JE
        | JGE -> JL  | JLE -> JG
        | JMP -> failwithf ""

type ABCD = 
    A | B | C | D
    override t.ToString() = match t with A -> "a" | B -> "b" | C -> "c" | D -> "d"

type RegType = L | H | X | EX
module RegType =
    let stringWith abcd typ = 
        abcd |>
        match typ with 
        | L -> sprintf "%Ol"
        | H -> sprintf "%Oh"
        | X -> sprintf "%Ox"
        | EX -> sprintf "e%Ox"
    let contains typ typ2 = 
        match typ2 with 
        | EX -> true
        | X -> typ <> EX
        | H | L -> typ = typ2
    let size = function L | H -> Byte | X -> Word | EX -> DWord
    let fromSize (low: RegType) (size: Size) = match size with Void -> invalidArg "There is no register of size Void!" "size" | Byte -> low | Word -> X | DWord -> EX 

///<summary>An assembly register - or global variable</summary>
type Register = 
    | ABCDReg of ABCD * RegType
    | DI  | BP  | SP  
    ///<summary>An assembly global variable (x db 2)</summary>
    | Var of string * Size
    override t.ToString () =
        match t with
        | ABCDReg (abcd, typ) -> RegType.stringWith abcd typ
        | SP -> "sp"
        | BP -> "bp"
        | DI -> "di"
        | Var (id,_) -> sprintf "[%s]" id
module Register =
    let size = function
        | ABCDReg (_, t) -> RegType.size t
        | DI | BP | SP -> Word
        | Var (_,s) -> s
    /// default for byte is L
    let fromSize x s = ABCDReg (x, RegType.fromSize L s) 

    let contains r1 r2 =
        match r1, r2 with
        | _ when r1 = r2 -> true
        | ABCDReg (abcd,typ), ABCDReg(abcd',typ') ->
            abcd = abcd' && RegType.contains typ typ'
        | _ -> false

    /// Do 2 registers share memory?
    let share r1 r2 =
        match r1, r2 with
        | _ when r1 = r2 -> true
        | ABCDReg (abcd, _), ABCDReg (abcd', _) -> abcd = abcd'
        | _ -> false

    let (|Contains|_|) r1 r2 = if contains r1 r2 then Some() else None

    module Registers =
        let [al; ah; ax; eax] = [L; H; X; EX] |> List.map (fun t -> ABCDReg(A,t)) 
        let [bl; bh; bx; ebx] = [L; H; X; EX] |> List.map (fun t -> ABCDReg(B,t)) 
        let [cl; ch; cx; ecx] = [L; H; X; EX] |> List.map (fun t -> ABCDReg(C,t)) 
        let [dl; dh; dx; edx] = [L; H; X; EX] |> List.map (fun t -> ABCDReg(D,t)) 
        let basicRegisters = [
            al; ah; ax; eax
            bl; bh; bx; ebx
            cl; ch; cx; ecx
            dl; dh; dx; edx
        ]
         
///<summary>An assembly operand</summary>
type [<StructuralEquality; NoComparison>] Operand = 
    | Constent of Literal
    | Reg of Register
    | Index of start: Register * offset: Literal * add: bool * Size
    override t.ToString() =
        match t with
        | Constent c -> string c
        | Reg r -> string r
        | Index (r, UInt 0u, _, s) -> sprintf "[%O ptr %O]" s r
        | Index (r, o, positive, s) -> sprintf "[%O ptr %O%c%O]" s r (if positive then '+' else '-') o
module Operand =
    let size = function
        | Constent c -> Literal.size c
        | Index (_,_,_,s) -> s
        | Reg r -> Register.size r
    //let contains r o =
    //    match o with
    //    | Constent _ -> false
    //    | Reg r' | Index (r', _, _, _) -> r' = r
    let uses r o =
        match o with
        | Constent _ -> false
        | Reg r' | Index (r', _, _, _) -> Register.share r r'

///<summary>An assembly instruction or label or such. Anything that will be a single line</summary>
type Line = 
    | Inst of string * Operand list
    | Jump of JumpType * Label
    ///<summary>Calls a procedure</summary>
    | Call of string
    | Comment of string
    | Label of Label
    | Ret of int
    /// An empty line - for generating clear complied code
    | EmptyLine
    | IndentIn 
    | IndentOut 
    /// A line that gets translated to the text inside
    | Text of string

type lines = Line list

module Line =
    let comment str = Comment str
    let make instruction (operands: Operand seq) = Inst(instruction, List.ofSeq operands)
    let mov x y = make "mov" [Reg x; y]
    let mov0 x = mov x (Constent (UInt 0u))
    let rec push (ABCDReg (abcd, typ) as reg) =        
        match typ with
        | L -> [mov0 (ABCDReg (abcd, H)); make "push" [Reg (ABCDReg (abcd, X))]]
        | H -> mov (ABCDReg (abcd, L)) (Reg reg) :: push (ABCDReg(abcd, L))
        | X | EX -> [make "push" [Reg reg]]
        //| EX -> [make "push" [Reg (ABCDReg(abcd,X))]; make "shr" [Reg (ABCDReg(abcd,EX)); Constent (UInt 16u)]; make "push" [Reg (ABCDReg(abcd,X))]]
    let pop (ABCDReg (abcd, typ) as reg) =        
        match typ with
        | L -> [make "pop" [Reg (ABCDReg (abcd, X))]]
        | H -> [make "pop" [Reg (ABCDReg (abcd, X))]; mov reg (Reg <| ABCDReg (abcd, L))]
        | X | EX -> [make "pop" [Reg reg]]
        //| EX -> [make "pop" [Reg (ABCDReg(abcd,X))]; make "shl" [Reg (ABCDReg(abcd,EX)); Constent (UInt 16u)]; make "pop" [Reg (ABCDReg(abcd,X))]]

///<summary>An assembly procedure. proc MyProc ... ret endp</summary>
type Procedure = { Name: string; Body: lines; Sig: Size * Size list }

///<summary>An assembly program with a stack data and code segment</summary>
type Program = { StackSize:int; Data: list<string * Size * Literal list>; Code: Procedure list }


