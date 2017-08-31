namespace Filomena.Backend.ScriptParser

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

type Platform = class end

type ('a, 'b) Maybe = Succeeded of 'a | FailedWith of 'b with
    static member Bind m f = 
        match m with
        | Succeeded x -> f m
        | FailedWith e -> m


module Parser =
    let checker = FSharpChecker.Create(keepAssemblyContents=true)

    let getUntypedTreeFromFile file input = 
        let projOptions, errors = 
            checker.GetProjectOptionsFromScript(file, input)
            |> Async.RunSynchronously

        let parseFileResults = 
            checker.ParseFileInProject (file, input, projOptions) 
            |> Async.RunSynchronously in
        match parseFileResults.ParseTree with
        | Some tree -> Succeeded tree
        | None -> FailedWith (sprintf "Errors: %A" parseFileResults.Errors)

    let getUntypedTree input = 
        let file = Path.ChangeExtension (Path.GetTempFileName (), "fsx") in
        do File.WriteAllText (file, input)
        getUntypedTreeFromFile file input

    let getUntypedTreeNoSettings input = 
        let file = Path.ChangeExtension (Path.GetTempFileName (), "fsx") in
        do File.WriteAllText (file, String.Empty)
        getUntypedTreeFromFile file input

    let visitDeclarations decls = 
        for decl in decls do 
            match decl with 
            | SynModuleDecl.Let(isRecursive, bindings, range) ->
                do printf "let "
                if isRecursive then do printf "rec "
                do printfn "%A" bindings
            | SynModuleDecl.Open(longIdentWithDots, range) -> 
                do printfn "Open %A" longIdentWithDots 
            | SynModuleDecl.DoExpr(sequencePointInfo, expression, range) ->
                do printfn "do %A" expression
                do printfn "with sequence point info %A" sequencePointInfo
            | _ -> ()

    let visitModulesAndNamespaces modulesOrNss =
        for moduleOrNs in modulesOrNss do 
            let (SynModuleOrNamespace(longIdent, isRecursive, isModule, moduleDecls, xmlDoc, attrs, accessibility, entityRange)) = moduleOrNs
            if isModule then do printf "Module " else do printf "Namespace "
            do printfn "%A" longIdent
            do visitDeclarations moduleDecls) 

    let parseScript source =
        let maybeTree = getUntypedTree source in
        match maybeTree with
        | Succeeded tree ->
            match tree with
            | ParsedInput.ImplFile implFile ->
                //do printfn "%A" implFile
                let (ParsedImplFileInput(fileName, isScript, name, scopedPragma, parsedHashDs, modules, (isLastCompiland, isExe))) = implFile in
                visitModulesAndNamespaces modules
            | ParsedInput.SigFile file ->
                do printfn "No support for interface files"
        | FailedWith error -> 
            do Console.WriteLine error
    
    
    
