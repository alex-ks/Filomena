namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler.SourceCodeServices

open Filomena.Backend.Models
open Filomena.Backend.Parsing.ProjectHelper

module TypedParser = 
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let defaultFileVersion = 0
    
    let getProjectOptions fileName source = 
        let projOptions, errors = 
            checker.GetProjectOptionsFromScript (fileName, source)
            |> Async.RunSynchronously in
        if errors.IsEmpty then
            Ok projOptions
        else
            Failed (List.toArray errors)

    let getTypedTreeFromProject fileName source =
        match getProjectOptions fileName source with
        | Ok projOptions ->
            let answer = 
                projOptions
                |> checker.ParseAndCheckProject
                |> Async.RunSynchronously in
            Ok answer
        | Failed errors -> Failed errors
            
    let checkSingleFileFromScript fileName source = 
        match getProjectOptions fileName source with
        | Ok projOptions ->
            let _, answer = 
                (fileName, defaultFileVersion, source, projOptions)
                |> checker.ParseAndCheckFileInProject
                |> Async.RunSynchronously in
            match answer with
            | FSharpCheckFileAnswer.Succeeded checkResults ->
                Ok checkResults                
            | FSharpCheckFileAnswer.Aborted ->
                Failed [||]
        | Failed errors -> Failed errors 
    
    let getTypedTree source = getTypedTreeFromProject (projectFromScript source) source

    let getTypedTreeNoSettings source = getTypedTreeFromProject (emptyProject ()) source
    
    let checkSingleFile source = checkSingleFileFromScript (projectFromScript source) source
    
    let checkSingleFileNoSettings source = checkSingleFileFromScript (emptyProject ()) source

    let rec visitExpression graph expr = Ok ()

    let (|Function|) (x: FSharpMemberOrFunctionOrValue) = x.FullType.IsFunctionType

    let visitDeclarations graph declaration = 
        match declaration with
        | FSharpImplementationFileDeclaration.Entity (entity, subDecls) ->
            Failed "Nested types or modules are not allowed"
        | FSharpImplementationFileDeclaration.InitAction (expr) ->
            visitExpression graph expr
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (funcOrVal, funcOrValss, expression) ->
            
            Ok ()
            
    let (|Module|) (x: FSharpEntity) = if x.IsFSharpModule then Some x else None
    
    let treeToComputationGraph (implFile: FSharpImplementationFileContents) =
        match implFile.Declarations with
        | [Entity(Module _, subdecls)] ->
            subdecls
            //|> List.map (ComputationGraph.empty () |> visitDeclarations)
            |> List.map (visitDeclarations <| ComputationGraph.empty ())
            |> List.reduce (fun accum next -> Ok ()) // TODO: implement
        | [_] -> 
            Failed "Top-level declaration can be module only"
        | [] ->
            Failed "Empty file"
        | _ ->
            Failed "Too many declarations"
        