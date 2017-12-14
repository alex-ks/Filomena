module Program

open Filomena.Backend.Parsing
open Microsoft.FSharp.Compiler.SourceCodeServices
open System

let print x = printfn "%A" x

[<EntryPoint>]
let main args =
    let source = if Array.length args > 0 then System.IO.File.ReadAllText(args.[0]) else """
module A
do printfn "Hello, world!"
"""
    //printfn "%A" (UntypedParser.parseAndCheckScript source)
    do print source
    let checkResult = TypedParser.checkSingleFile source
    let partialAssemblySign = checkResult.PartialAssemblySignature
    
    let [moduleA] = partialAssemblySign.Entities |> Seq.toList
    
    let projCheckResult = TypedParser.getProjectTypedTree source 
    
    let [scriptFile] = projCheckResult.AssemblyContents.ImplementationFiles

    try
        let parsedProgram = TypedParser.parseProgramTree scriptFile
        do print parsedProgram
    with
    | UnexpectedException msg
    | NotSupportedException msg ->
        do printfn "%s" msg
    | :? NotImplementedException as e ->
        do printfn "%s" e.Message

    match scriptFile.Declarations with
    | [FSharpImplementationFileDeclaration.Entity (entity, declList)] ->
        do print (entity.MembersFunctionsAndValues |> Seq.toList)
        do print (declList |> Seq.toList)
        declList
        |> List.map (fun x ->
            match x with 
            | MemberOrFunctionOrValue (valOrf, valOfss, expr) ->
                let isFunc = valOrf.FullType.IsFunctionType
                do printfn "Full name: %s, Compiled name: %s, is function: %b" valOrf.FullName valOrf.CompiledName isFunc
                do printfn "Parameters: %A" valOfss
            | _ -> 
                do ignore ())
        |> ignore
        ()
    | [FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (mfv, mfvll, expr)] ->
        ()
    | [FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (mfv, mfvll, expr); _] ->
        ()
    | _ ->
        ()
    0