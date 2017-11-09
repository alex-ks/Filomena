module Program

open Filomena.Backend.Parsing
open Microsoft.FSharp.Compiler.SourceCodeServices

let print x = printfn "%A" x

[<EntryPoint>]
let main args =
    let source = if Array.length args > 0 then System.IO.File.ReadAllText(args.[0]) else """
module A
do printfn "Hello, world!"
"""
    //printfn "%A" (UntypedParser.parseAndCheckScript source)
    let (Ok checkResult) = TypedParser.checkSingleFileNoSettings source
    let partialAssemblySign = checkResult.PartialAssemblySignature
    let [moduleA] = partialAssemblySign.Entities |> Seq.toList
    //let [myAddFunc] = moduleA.MembersFunctionsAndValues |> Seq.toList
    //print myAddFunc.FullType
    
    let (Ok projCheckResult) = TypedParser.getTypedTree source 
    
    let [scriptFile] = projCheckResult.AssemblyContents.ImplementationFiles
    match scriptFile.Declarations with
    | [FSharpImplementationFileDeclaration.Entity (entity, declList)] ->
        do print (entity.MembersFunctionsAndValues |> Seq.toList)
        do print (declList |> Seq.toList)
        declList
        |> List.map (fun x ->
            match x with 
            | MemberOrFunctionOrValue (valOrf, valOfss, expr) ->
                do printfn "Full name: %s, Compiled name: %s" valOrf.FullName valOrf.CompiledName
                let rec visitExpr expr = 
                    match expr with
                    | BasicPatterns.Let ((bindVar, bindExpr), bodyExpr) ->
                        do printfn "Full name: %s, Compiled name: %s" bindVar.FullName bindVar.CompiledName
                        do visitExpr bindExpr
                        do visitExpr bodyExpr
                        ()
                    | _ -> ()
                do visitExpr expr                
                //do printfn "%A: %A of %A" valOrf valOfss valOrf.FullType
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