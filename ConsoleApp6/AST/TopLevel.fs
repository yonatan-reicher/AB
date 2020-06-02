[<AutoOpen>]
module Asmb.AST.TopLevel

open Asmb

type BiOperator = 
    | Add | Sub | Mul | Div | Mod
    | EQ | Lesser | Greater | NEQ | NLesser | NGreater | And | Or
module BiOperator =
    let (|Equation|Arithmetic|) = function 
        | EQ | Lesser | Greater | NEQ 
        | NLesser | NGreater | And | Or -> Equation
        | Add | Sub | Mul | Div | Mod -> Arithmetic
    let size x s1 s2 = 
        match x with
        | Arithmetic -> Size.max s1 s2
        | Equation -> Byte
type UOperator =
    | PointerOf | PointerVal  
    | Not
module UOperator = 
    let inline size x (s: Size) = 
        match x with
        | PointerOf -> Word
        | PointerVal -> DWord
        | Not -> Byte

type Expr =
    | Constent of Literal
    | Variable of string
    | BiOperation of BiOperator*Expr*Expr
    | UOperation of UOperator*Expr
    | Convert of Expr*Size
    | Call of string*Expr list
    override t.ToString () = 
        match t with
        | Constent c -> string c
        | Variable name -> name
        | BiOperation (o, e1, e2) -> sprintf "(%O %A %O)" e1 o e2
        | UOperation (o, e1) -> sprintf "%A%O" o e1
        | Convert (e, s) -> sprintf "(%O as %O)" e s
        | Call (name, param) -> sprintf "%s(%s)" name (param |> List.map string |> String.concat ", ")

module Expr = 
    ///<summary>Returns the size of the value that the expression will return</summary>
    let rec size (vars: seq<string*Size>, procs: seq<string*ProcSig>) = function
        | Variable name -> Seq.find (fst >> (=) name) vars |> snd
        | Constent c -> Literal.size c
        | BiOperation (o,e1,e2) -> BiOperator.size o (size (vars, procs) e1) (size (vars, procs) e2)
        | UOperation (o,e) -> UOperator.size o (size (vars, procs) e)
        | Convert (_,s) -> s        
        | Call (name,_) -> Seq.find (fst >> (=) name) procs |> snd |> fst

    let children = function
        | Variable _ 
        | Constent _ -> Seq.empty
        | BiOperation (_, e1, e2) -> seq {e1; e2}
        | UOperation (_, e1) -> seq {e1}
        | Convert (e1, _) -> seq {e1}
        | Call (_, param) -> List.toSeq param

    let rec functions expr = 
        let start = match expr with Call (name, _) -> seq {name} | _ -> Seq.empty
        Seq.append start (children expr |> Seq.collect functions)

type Statement =
    | Pushpop of Expr list * Block
    | IfElse of cond: Expr * trueBlock: Block * falseBlock: Block
    | While of Expr * Block
    | Assign of Expr * Expr
    | SideEffect of Expr
    | StackDeclare of string * Size * Expr option
    | Comment of string
    | Return of Expr option
    | UnsafePush of Expr
    | UnsafePop of Expr
    | NativeAssemblyLines of string []
    override t.ToString() =
        match t with
        | Pushpop (exprs, _) -> exprs |> List.map string |> String.concat ", " |> sprintf "pushpop %s"
        | IfElse (cond, _, _) -> sprintf "if %O" cond
        | While (expr, _) -> sprintf "while %O" expr
        | Assign (e1, e2) -> sprintf "%O <- %O" e1 e2
        | SideEffect e -> string e
        | StackDeclare (name, size, Some expr) -> sprintf "%s %O = %O" name size expr
        | StackDeclare (name, size, None) -> sprintf "%s %O" name size
        | Comment comment -> sprintf "//  %O" comment
        | Return (Some expr) -> sprintf "return %O" expr
        | Return None -> "return"
        | UnsafePush e -> sprintf "push# %O" e
        | UnsafePop e -> sprintf "pop# %O" e
        | NativeAssemblyLines lines -> String.concat "\n" lines

and Block = 
    | Block of comment: string option * Statement list
    member t.Comment = match t with Block (comment,_) -> comment

module Statement =
    /// A sequence containing all the expressions in a statement
    let rec exprs = function
        | Pushpop(exprs, block) -> Seq.append exprs (blockExprs block)
        | IfElse(cond, trueBlock, falseBlock) -> 
            Seq.append [cond] (blockExprs trueBlock) |> Seq.append (blockExprs falseBlock)
        | While(expr, block) -> Seq.append [expr] (blockExprs block)
        | Assign(expr1, expr2) -> seq {expr1; expr2}
        | SideEffect expr 
        | Return(Some expr)
        | UnsafePush expr
        | UnsafePop expr 
        | StackDeclare(_,_,Some expr) -> seq {expr}
        | Return None
        | StackDeclare(_,_,None)
        | NativeAssemblyLines _
        | Comment _ -> Seq.empty
    and blockExprs (Block (_, statements)) = Seq.collect exprs statements     

    let functions = exprs >> Seq.collect Expr.functions
    let blockFunctions = blockExprs >> Seq.collect Expr.functions
 
type AsmbProcedure = 
    {  ProcName: string
       ProcBody: Block
       Parameters: (string * Size) list
       RetSize: Size                       }
    member t.Sig: ProcSig = t.RetSize, List.map snd t.Parameters

type AsmbProgram = {    ProgVariables: (string * Size * Literal list) list
                        ProgProcedures: AsmbProcedure list               }
