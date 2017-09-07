namespace Filomena.Backend.ScriptParser

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

type Platform = class end

type ('a, 'b) Maybe = Succeeded of 'a | FailedWith of 'b with
    static member Bind m f = 
        match m with
        | Succeeded x -> f x
        | FailedWith e -> m

module Parser =
    let identToString (ident: LongIdentWithDots) = ident.Lid |> List.map (fun x -> x.idText) |> String.concat "."

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

    let visitPattern pattern = printf "pattern"
    
    let visitExpression expr = printf "expr"
    
    let rec typeToString = function
    | SynType.LongIdent (ident) -> identToString ident  
    | SynType.Array (n, elementType, range) -> typeToString elementType + "[]"
    | SynType.Fun (argType, returnType, range) -> typeToString argType + "->" + typeToString returnType
    | SynType.Tuple (typeNames, range) -> typeNames |> List.map (fun x -> snd x) |> List.map typeToString |> String.concat " * "
    | SynType.Var (Typar(genericName, staticReq, isComplGenerated), range) -> genericName.idText // Generic type placeholder
    | _ -> "unknown"

    let visitDeclarations decls = 
        for decl in decls do 
            match decl with 
            | SynModuleDecl.Let(isRecursive, bindings, range) ->
                do printf "let "
                if isRecursive then do printf "rec "
                for binding in bindings do
                    let (Binding (access, kind, isInline, isMutable, attrs, xmlDoc, data, pattern, retInfo, body, range, sequencePointInfo)) = binding
                    do visitPattern pattern
                    match retInfo with
                    | Some (SynBindingReturnInfo(synType, range, attrs)) ->
                        printf " : %s" (typeToString synType)
                    | None -> ()
                    do printf " = "
                    do visitExpression body
                    printfn ""
                // do printfn "%A" bindings
            | SynModuleDecl.Open(longIdentWithDots, range) ->
                do printfn "open %s" (longIdentWithDots.Lid |> List.map (fun x -> x.idText) |> String.concat ".")
                
            | SynModuleDecl.DoExpr(sequencePointInfo, expression, range) ->
                do printfn "do %A" expression
                do printfn "with sequence point info %A" sequencePointInfo
            | _ -> ()

    let visitModulesAndNamespaces modulesOrNss =
        for moduleOrNs in modulesOrNss do 
            let (SynModuleOrNamespace(longIdent, isRecursive, isModule, moduleDecls, xmlDoc, attrs, accessibility, entityRange)) = moduleOrNs
            if isModule then do printf "module " else do printf "namespace "
            do printfn "%A" longIdent
            do visitDeclarations moduleDecls

    let parseScript source =
        let maybeTree = getUntypedTree source in
        match maybeTree with
        | Succeeded tree ->
            match tree with
            | ParsedInput.ImplFile implFile ->
                //do printfn "%A" implFile
                let (ParsedImplFileInput(fileName, isScript, name, scopedPragma, parsedHashDs, modules, (isLastCompiland, isExe))) = implFile in
                do visitModulesAndNamespaces modules
            | ParsedInput.SigFile file ->
                do printfn "No support for interface files"
        | FailedWith error -> 
            do Console.WriteLine error

