namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler.SourceCodeServices

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

