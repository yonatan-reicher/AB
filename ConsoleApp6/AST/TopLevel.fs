[<AutoOpen>]
module Asmb.AST.TopLevel

open Asmb

type BiOperator = 
    | Add | Sub | Mul | Div | Mod
    | EQ | Lesser | Greater | NEQ | NLesser | NGreater | And | Or
module BiOperator =
    let size x s1 s2 = 
        match x with
        | Add | Sub | Mul | Div | Mod -> Size.max s1 s2
        | EQ | Lesser | Greater | NEQ | NLesser | NGreater | And | Or -> Byte
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

and Statement =
    | Pushpop of Expr list * Block
    | IfElse of cond: Expr * trueBlock: Block * falseBlock: Block
    | While of Expr * Block
    | Assign of Expr * Expr
    | StackDeclare of string * Size * Expr option
    | Comment of string
    | Return of Expr option
    | NativeAssemblyLines of string []
    override t.ToString() =
        match t with
        | Pushpop (exprs, _) -> exprs |> List.map string |> String.concat ", " |> sprintf "pushpop %s"
        | IfElse (cond, _, _) -> sprintf "if %O" cond
        | While (expr, _) -> sprintf "while %O" expr
        | Assign (e1, e2) -> sprintf "%O <- %O" e1 e2
        | StackDeclare (name, size, Some expr) -> sprintf "%s %O = %O" name size expr
        | StackDeclare (name, size, None) -> sprintf "%s %O" name size
        | Comment comment -> sprintf "//  %O" comment
        | Return (Some expr) -> sprintf "return %O" expr
        | Return None -> "return"
        | NativeAssemblyLines lines -> String.concat "\n" lines
    
and Block = 
    | Block of comment: string option * Statement list
    member t.Comment = match t with Block (comment,_) -> comment
 
type AsmbProcedure = 
    {  ProcName: string
       ProcBody: Block
       Parameters: (string * Size) list
       RetSize: Size                       }
    member t.Sig: ProcSig = t.RetSize, List.map snd t.Parameters

type AsmbProgram = {    ProgVariables: (string * Size * Literal list) list
                        ProgProcedures: AsmbProcedure list               }

module Expr =
    ///<summary>Returns the size of the value that the expression will return</summary>
    let rec size (vars: seq<string*Size>, procs: seq<string*ProcSig>) = function
        | Variable name -> Seq.find (fst >> (=) name) vars |> snd
        | Constent c -> Literal.size c
        | BiOperation (o,e1,e2) -> BiOperator.size o (size (vars, procs) e1) (size (vars, procs) e2)
        | UOperation (o,e) -> UOperator.size o (size (vars, procs) e)
        | Convert (_,s) -> s        
        | Call (name,_) -> Seq.find (fst >> (=) name) procs |> snd |> fst