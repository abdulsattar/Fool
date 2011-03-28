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
        if System.String.Compare(System.IO.Path.GetExtension(fn), ".fool", true) <> 0 then
            printfn "WARNING: You better use FOOL as your extension"

        printfn "Lexing..."
        let lexbuf = LexBuffer<char>.FromTextReader (new System.IO.StreamReader(System.IO.Path.Combine(System.Environment.CurrentDirectory, fn)))

        printfn "Parsing..."
        let program = Parser.Prog Lexer.tokenize lexbuf

        printfn "Generating code..."
        generate(program, System.IO.Path.GetFileNameWithoutExtension(fn))


    with ex ->
        printfn "Unhandled Exception: %s" ex.Message

    0