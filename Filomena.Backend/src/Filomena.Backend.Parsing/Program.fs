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
module MyFiltering

open SI
open Eeg

let raw = loadEeg "R013_raw"
let raw' = dropChannel "NOSE" raw
let filtered = filterFrequences 0.1<Hz> 40.<Hz> raw'
"""
    let optSource = if Array.length args > 1 then
                        System.IO.File.ReadAllText args.[1]
                    else """
module MyModule

let print (str: string) = ignore str
"""

    do print source

    try
        let compiler = ResolverRestClient "http://localhost:7945" |> Compiler
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