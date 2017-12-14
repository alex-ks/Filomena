namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler.SourceCodeServices

open Filomena.Backend.Models
open ProjectHelper
open Exceptions

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

    let rec typeToModel (t: FSharpType) =
        let name = t.TypeDefinition.FullName
        let parameters = 
            if t.GenericArguments.Count = 0 then None 
            else Some (t.GenericArguments |> Seq.map typeToModel |> Seq.toList) in
        { name = name; parameters = parameters }

    let (|Reversed|) lst = List.rev lst

    let uniqueName () = System.Guid.NewGuid () |> sprintf "%A"

    let serializeConst (x: System.Object) = string x // TODO: check if "string" serialization is correct

    let rec visitExpression (nameOpt: string option) 
                            predcessors
                            (parsedProgram: ParsedProgram) 
                            (expr: FSharpExpr) =
        match expr with
        | BasicPatterns.Sequential (expr1, expr2) ->
            let updatedProgram, depOpt = visitExpression None predcessors parsedProgram expr1
            let predcessors' = 
                match depOpt with
                | Some op -> predcessors |> Set.add op
                | None -> predcessors
            visitExpression nameOpt predcessors' updatedProgram expr2
            
        | BasicPatterns.Let ((bindVar, bindExpr), bodyExpr) ->
            let updatedProgram, _ = visitExpression (Some bindVar.CompiledName) predcessors parsedProgram bindExpr
            visitExpression nameOpt predcessors updatedProgram bodyExpr

        | BasicPatterns.Call (objExprOpt, memberOrFunc, _typeArgs1, _typeArgs2, argsExprs) ->
            match objExprOpt with
            | Some _ ->
                "Object method calling" |> notSupported
            | None ->
                let processArgumentExpr i argExpr = 
                    match argExpr with
                    | BasicPatterns.Value value -> 
                        ProgramDiff.empty, value.CompiledName
                    | _ -> 
                        let argName = ParsedProgram.escapeName parsedProgram (memberOrFunc.CompiledName + (string i))
                        let updatedProgram, _ = visitExpression (Some argName) predcessors parsedProgram argExpr in
                        (updatedProgram - parsedProgram), argName
                let updatedProgram, (Reversed argNames) = 
                    argsExprs
                    |> List.mapi processArgumentExpr
                    |> List.fold (fun (program, names) (diff, mnemo) ->
                        program + diff, mnemo :: names) (parsedProgram, [])
                let name = 
                    match nameOpt with 
                    | Some str -> str 
                    | None -> ParsedProgram.escapeName updatedProgram (memberOrFunc.FullName + "Output")
                let dependencies = 
                    argNames
                    |> List.filter (fun name -> 
                        match updatedProgram.mnemonics.[name] with
                        | MnemonicOrigin.Output _ -> true
                        | _ -> false)
                    |> List.map (fun name ->
                        match updatedProgram.mnemonics.[name] with
                        | MnemonicOrigin.Output operation -> 
                            operation
                        | _ -> 
                            unexpected "There must be only operation outputs")
                    |> Set.ofList
                let operation = { name = memberOrFunc.FullName;
                                  inputs = argNames;
                                  output = name;
                                  dependencies = Set.union dependencies predcessors } in
                updatedProgram 
                |> ParsedProgram.addMnemonic name (MnemonicOrigin.Output operation), (Some operation)
                
        | BasicPatterns.Value value ->
            let updatedProgram = 
                match nameOpt with
                | Some name -> 
                    parsedProgram
                    |> ParsedProgram.addMnemonic name (MnemonicOrigin.Alias value.CompiledName)
                | None ->
                    parsedProgram
            updatedProgram, None

        | BasicPatterns.Const (objVal, fsType) ->
            let dataType = typeToModel fsType
            let name = 
                match nameOpt with
                | Some str -> str
                | None -> ParsedProgram.escapeName parsedProgram (dataType.name + "val")
            let updatedProgram = 
                parsedProgram
                |> ParsedProgram.addMnemonic name (Const (serializeConst objVal, dataType))
            updatedProgram, None

        | _ ->
            notSupported "Expression is not supported"

    let (|Function|) (x: FSharpMemberOrFunctionOrValue) = x.FullType.IsFunctionType

    let rec visitDeclarations program predcessors declarations = 
        match declarations with
        | [] ->
            program
        | _ ->
            let decl, tail = List.head declarations, List.tail declarations
            match decl with
            | Entity _ ->
                failwith "Nested types or modules are not allowed"

            | InitAction expr ->
                let updatedProgram, depOpt = visitExpression None predcessors program expr
                let predcessors' = 
                    match depOpt with
                    | Some op -> predcessors |> Set.add op
                    | None -> predcessors
                visitDeclarations updatedProgram predcessors' tail

            | MemberOrFunctionOrValue (funcOrVal, _, expression) ->
                if (|Function|) funcOrVal then
                    "Functions declaration" |> notSupported
                else
                    let updatedProgram, _ = 
                        visitExpression (Some funcOrVal.CompiledName) predcessors program expression
                    visitDeclarations updatedProgram predcessors tail
            
    let (|Module|) (x: FSharpEntity) = if x.IsFSharpModule then Some x else None
    
    let parseProgramTree (implFile: FSharpImplementationFileContents) =
        match implFile.Declarations with
        | [Entity(Module _, subdecls)] ->
            visitDeclarations ParsedProgram.empty Set.empty subdecls
        | [_] -> 
            failwith "Top-level declaration can be module only"
        | [] ->
            failwith "Empty file"
        | _ ->
            failwith "Too many declarations"
        