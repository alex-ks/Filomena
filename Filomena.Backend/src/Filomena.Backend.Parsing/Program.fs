module Program

open Filomena.Backend.Parsing
open Microsoft.FSharp.Compiler.SourceCodeServices
open System

let print x = printfn "%A" x

[<EntryPoint>]
let main args =
    let source = if Array.length args > 0 then
                     System.IO.File.ReadAllText(args.[0]) 
                 else """
module A

open MyModule

do print "Hello, world!"
"""
    let optSource = if Array.length args > 1 then
                        System.IO.File.ReadAllText args.[1]
                    else """
module MyModule

let print (str: string) = ignore str
"""

    do print source

    try
        let parsedProgram, errors = TypedParser.parse [optSource] source
        do print parsedProgram
        do print errors
    with
    | UnexpectedException msg | NotSupportedException msg ->
        do printfn "%s" msg
    | CheckException errors ->
        do print errors
    | :? NotImplementedException as e ->
        do printfn "%s" e.Message
    0