namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler.SourceCodeServices

open Filomena.Backend.Parsing.ProjectHelper

module TypedParser = 
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let defaultFileVersion = 0

    let getTypedTreeFromProject fileName source = 
        let projOptions, errors = 
            checker.GetProjectOptionsFromScript (fileName, source)
            |> Async.RunSynchronously in
        if errors.Length = 0 then
            let _, answer = 
                ((fileName, defaultFileVersion, source, projOptions)
                |> checker.ParseAndCheckFileInProject
                |> Async.RunSynchronously) in
            match answer with
            | FSharpCheckFileAnswer.Succeeded checkResults ->
                Ok checkResults                
            | FSharpCheckFileAnswer.Aborted ->
                Failed [||]
        else
            Failed (List.toArray errors)
    
    let getTypedTree source = getTypedTreeFromProject (projectFromScript source) source

    let getTypedTreeNoSettings source = getTypedTreeFromProject (emptyProject ()) source

    let pr = getTypedTree "printfn \"Hello, world!\""
