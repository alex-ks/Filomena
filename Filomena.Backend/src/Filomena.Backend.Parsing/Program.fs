module Program

open Filomena.Backend.Parsing
open Microsoft.FSharp.Compiler.SourceCodeServices
open System
open Filomena.Backend.ResolverClient

let print x = printfn "%A" x

[<EntryPoint>]
let main args =
    let source = if Array.length args > 0 then
                     System.IO.File.ReadAllText(args.[0]) 
                 else """
module A

open SI

let a = 1. / 1.<s>
"""
    let optSource = if Array.length args > 1 then
                        System.IO.File.ReadAllText args.[1]
                    else """
module MyModule

let print (str: string) = ignore str
"""

    do print source

    try
        let compiler = ResolverRestClient "http://ecclesia.ict.nsc.ru:27945" |> Compiler
        let graph = (compiler.Compile source).Result
        do print graph.mnemonicsTable
    with
    | UnexpectedException msg | NotSupportedException msg ->
        do printfn "%s" msg
    | CheckException errors ->
        do print errors
    | :? NotImplementedException as e ->
        do printfn "%s" e.Message
    0