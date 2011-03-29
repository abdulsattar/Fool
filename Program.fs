open System
open Microsoft.FSharp.Text.Lexing

open Ast
open Lexer
open Parser
open Codegen

[<EntryPoint>]
let main(args : string[]) =
    
    try
        if args.Length <> 1 then failwith "Usage Fool <filename>"
        let fn = args.[0]
        if System.String.Compare(System.IO.Path.GetExtension(fn), ".fl", true) <> 0 then
            printfn "WARNING: You better use \"fl\" as your extension"

        let lexbuf = LexBuffer<char>.FromTextReader (new System.IO.StreamReader(System.IO.Path.Combine(System.Environment.CurrentDirectory, fn)))

        let program = 
            try
                Parser.Prog Lexer.tokenize lexbuf
            with
            | _ -> failwithf "Unrecognized parse error at (%d,%d)" lexbuf.EndPos.Line lexbuf.EndPos.Column

        printfn "Generating code..."
        generate(program, System.IO.Path.GetFileNameWithoutExtension(fn))

    with ex ->
        printfn "%s" ex.Message

    0
