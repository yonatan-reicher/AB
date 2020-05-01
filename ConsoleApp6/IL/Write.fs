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
    | Call name -> append <| sprintf "call %s" name
    | Comment str -> append <| sprintf ";\t%s" str
    | Jump (jmp, label) -> append <| sprintf "%O %O" jmp label
    | Label label -> append <| sprintf "%O:" label
    | Ret i -> append <| sprintf "ret %d" i
    | Text str -> append str

let writeLines (lines: lines) writer = List.fold (fun writer line -> writeLine line writer) writer lines

let writeProc { Name = name; Body = body; Sig = ``sig`` } =
       append (sprintf "proc %s" name)
    >> indented 1 (writeLines body)
    >> append (sprintf "endp %s" name)

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
        |> indented 1 
            (fun writer -> List.fold (fun writer (name, size, literals) ->
                let defSize = match size with Byte -> "db" | Word -> "dw" | DWord -> "dd"
                let strLiterals = List.map string literals |> String.concat ", "
                append (sprintf "%s %s %s" name defSize strLiterals) writer) writer vars)
        |> appendNewLine
        //      Code
        |> append ".386"
        |> append "LOCAL @@"
        |> append ".code"
        |> indented 1 (
               append "mov ax, @data"
            >> append "mov ds, ax"
            >> append "call main"
            >> append "pop dx"
            >> append "mov ah, 2"
            >> append "int 21h"
            >> append "exit:"
            >> indented 1 (
                   append "mov ax, 4C00h"
                >> append "int 21h")
            >> appendNewLine
            >> fun writer -> List.fold (fun writer p -> writeProc p writer |> appendNewLine) writer procs)
        |> append "end"
        |> str