open System
open Microsoft.FSharp.Text.Lexing

open Ast
open Lexer
open Parser
open Codegen

[<EntryPoint>]
let main(args : string[]) =
    
    try
        if args.Length <> 1 then failwith "Usage Kenny <filename>"
        let fn = args.[0]

        printfn "Lexing..."
        let lexbuf = LexBuffer<char>.FromTextReader (new System.IO.StreamReader(System.IO.Path.Combine(System.Environment.CurrentDirectory, fn)))
        
        printfn "Parsing..."
        let program = Parser.start Lexer.tokenize lexbuf

        printfn "Generating code..."
        generate(program, System.IO.Path.GetFileNameWithoutExtension(fn))


    with ex ->
        printfn "Unhandled Exception: %s" ex.Message

    0