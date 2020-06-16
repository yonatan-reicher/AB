namespace Asmb.AST

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
    /// Returns the size of the value that the expression will return 
    let rec size (getVar, getProc): _ -> Result<Size,_> = function
        | Variable name -> getVar name
        | Call (name,_) -> getProc name
        | Constent c -> Ok (Literal.size c)
        | BiOperation (o,e1,e2) -> 
            catch {
                let! s1 = size (getVar, getProc) e1
                let! s2 = size (getVar, getProc) e2
                return BiOperator.size o s1 s2
            }
        | UOperation (o,e) -> Result.map (UOperator.size o) (size (getVar, getProc) e)
        | Convert (_,s) -> Ok s

    let rec children expr = 
        match expr with
        | Variable _ 
        | Constent _ -> Seq.empty
        | BiOperation (_, e1, e2) -> seq {e1; e2}
        | UOperation (_, e1) -> seq {e1}
        | Convert (e1, _) -> seq {e1}
        | Call (_, param) -> List.toSeq param
        |> Seq.collect (fun e -> Seq.append [e] (children e)) 

    let rec functions expr = 
        Seq.append [expr] (children expr)
        |> Seq.choose (function Call (name, _) -> Some name | _ -> None)

type Statement =
    | Pushpop of Expr list * Block
    | IfElse of cond: Expr * trueBlock: Block * falseBlock: Block option
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

module rec Statement =
    /// A sequence containing all the expressions in a statement (Some are nested - use Expr.children)
    let rec exprs statement = 
        match statement with
        | Pushpop(exprs, block) -> Seq.append exprs (blockExprs block)
        | IfElse(cond, trueBlock, falseBlock) -> 
            Seq.append [cond] (blockExprs trueBlock) 
            |> match falseBlock with 
                | Some block -> Seq.append (blockExprs block)
                | None -> id
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
 
type Function = 
    {  Name: string
       Body: Block
       Parameters: (string * Size) list
       RetSize: Size    }
    member t.Sig: FuncSig = t.RetSize, List.map snd t.Parameters
module Function =
    let name ({ Name = name }: Function) = name
    let body ({ Body = body }: Function) = body
    let parameters ({ Parameters = parameters }: Function) = parameters
    let signature (f: Function) = f.Sig

type AsmbProgram = {    ProgVariables: (string * Size * Literal list) list
                        ProgFunctions: Function list               }
