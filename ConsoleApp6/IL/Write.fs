module Asmb.IL.Write

open Asmb
open Asmb.IL

type Writer = { CharStack: char list; Indent: int }
module Writer =
    let empty = { CharStack = []; Indent = 0 }
    let append str writer = 
        let tabs = String.replicate writer.Indent "\t"
        let chars = List.ofSeq (tabs + str + "\n")
        { writer with CharStack = List.rev chars @ writer.CharStack }
    let appendNewLine writer = {writer with CharStack = '\n' :: writer.CharStack}
    let str {CharStack = chars} = List.fold (fun s c -> sprintf "%c%s" c s) "" chars
    let indent n writer = { writer with Indent = writer.Indent + n }
    let indented n func writer = writer |> indent n |> func |> indent -n

open Writer

let writeLine (line: Line) =
    match line with
    | Inst (code, operands) -> 
        append <| sprintf "%s %s" code (Seq.map string operands |> String.concat ", ")
    | IndentIn -> indent 1
    | IndentOut -> indent -1
    | EmptyLine -> appendNewLine
    | Call name -> append <| sprintf "call %s" (notReserved name)
    | Comment str -> append <| sprintf ";\t%s" str
    | Jump (jmp, label) -> append <| sprintf "%O %O" jmp label
    | Label label -> append <| sprintf "%O:" label
    | Ret i -> append <| sprintf "ret %d" i
    | Text str -> append str

let writeLines (lines: lines) = func { for line in lines do do! writeLine line }

let writeProc { Name = name; Body = body } =
    func {
       do! append (sprintf "proc %s" (notReserved name))
       do! indented 1 (writeLines body)
       do! append (sprintf "endp %s" (notReserved name))
    }

let writeProg { Code = procs; Data = vars; StackSize = stack }: string =

    match List.tryFind (fun p -> p.Name = "main") procs with
    | None -> failwithf "Program has no main function what are you doing please add a main"
    | Some main ->
        if fst main.Sig <> Byte || snd main.Sig <> [] then failwithf "main must return byte and return no parameters"
        empty
        //      Model
        |> append ".model small"
        |> appendNewLine
        //      Stack
        |> append (sprintf ".stack %d" stack)
        |> appendNewLine
        //      Data
        |> append ".data"
        |> func {
            do! indented 1
            for name, size, literals in vars do
                let defSize = match size with Byte -> "db" | Word -> "dw" | DWord -> "dd"
                let strLiterals = List.map string literals |> String.concat ", "
                do! append (sprintf "%s %s %s" (notReserved name) defSize strLiterals)
        }
        |> appendNewLine
        //      Code
        |> append ".386"
        |> append "LOCALS @@"
        |> append ".code"
        |> indented 1 (
               append "mov ax, @data"
            >> append "mov ds, ax"
            >> append "call main"
            >> append "mov dx, ax"
            >> append "mov ah, 2"
            >> append "add dl, '0'"
            >> append "int 21h"
            >> append "exit:"
            >> indented 1 (
                   append "mov ax, 4C00h"
                >> append "int 21h")
            >> appendNewLine
            >> func { 
                for p in procs do 
                    do! writeProc p 
                    do! appendNewLine
            })
        |> append "end"
        |> str