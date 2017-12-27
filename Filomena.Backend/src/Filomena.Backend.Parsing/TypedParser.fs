namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler.SourceCodeServices

open ProjectHelper
open Exceptions

type ErrorSeverity = Warning | Error

type ParsingError = { ErrorNumber: int
                      StartLine: int
                      StartColumn: int
                      EndLine: int
                      EndColumn: int
                      Severity: ErrorSeverity
                      Subcategory: string
                      Message: string }

module TypedParser =
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let defaultFileVersion = 0
    
    let getProjectOptions fileName optNames = 
        let compilerParams = 
            [| yield "--simpleresolution" 
               yield "--noframework" 
               yield "--debug:full" 
               yield "--define:DEBUG" 
               yield "--optimize-" 
               yield "--out:" + (changeToDll fileName)
               yield "--doc:test.xml" 
               yield "--warn:3" 
               yield "--fullpaths" 
               yield "--flaterrors" 
               yield "--target:library"             
               for name in optNames -> name
               yield fileName
               let references =
                 [ sysLib "mscorlib" 
                   sysLib "System"
                   sysLib "System.Core"
                   sysLib "System.Runtime"
                   sysLib "System.Private.CoreLib"
                   fscorePath ]
               for r in references -> "-r:" + r |]
        let projectName = changeToFsproj fileName
        checker.GetProjectOptionsFromCommandLineArgs (projectName, compilerParams)

    let getTypedTreeFromProject fileName optNames =
        let projectOptions = getProjectOptions fileName optNames
        projectOptions
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    
    let dispose (x: System.IDisposable) = x.Dispose ()

    let getProjectTypedTree fileName source optSources = 
        let file = new TempFile (fileName, source)
        let optFiles = 
            optSources
            |> Seq.map (fun code -> new TempFile(tempFileName (), code))
            |> Seq.toList
        use _guard = { new System.IDisposable with
                       member __.Dispose () = for f in file::optFiles do dispose f }
        let optNames = 
            optFiles
            |> Seq.map (fun f -> f.Name)
        getTypedTreeFromProject (file.Name) optNames
    
    type DataType = Filomena.Backend.Models.DataType

    let rec typeToModel (t: FSharpType) =
        let name = 
            match t.TypeDefinition.TryFullName with
            | Some fullName -> fullName
            | None -> t.TypeDefinition.DisplayName

        let parameters = 
            if t.GenericArguments.Count = 0 then None 
            else Some (t.GenericArguments |> Seq.map typeToModel |> Seq.toList) in
        { DataType.name = name; DataType.parameters = parameters }

    let (|Reversed|) lst = List.rev lst

    let uniqueName () = System.Guid.NewGuid () |> sprintf "%A"

    let serializeConst (x: System.Object) = string x // TODO: check if "string" serialization is correct

    let findDependencies program argNames = 
        argNames
        |> List.filter (fun name -> 
            match program.mnemonics.[name] with
            | Output _ -> true
            | _ -> false)
        |> List.map (fun name ->
            match program.mnemonics.[name] with
            | Output operation -> 
                operation
            | _ -> 
                unexpected "There must be only operation outputs")
        |> Set.ofList

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

        | BasicPatterns.NewTuple (tupleType, argsExprs) ->
            let processArgumentExpr = processFuncArgs parsedProgram predcessors HiddenOps.NewTuple 
            let updatedProgram, (Reversed argNames) = 
                argsExprs
                |> List.mapi processArgumentExpr
                |> List.fold (fun (program, names) (diff, mnemo) ->
                    program + diff, mnemo :: names) (parsedProgram, [])
            let name = 
                match nameOpt with 
                | Some str -> str 
                | None -> ParsedProgram.escapeName updatedProgram (HiddenOps.NewTuple + "Output")
            let dependencies = findDependencies updatedProgram argNames
            let operation = { name = HiddenOps.NewTuple;
                              inputs = argNames;
                              output = name;
                              parameters = Some [typeToModel tupleType];
                              dependencies = Set.union dependencies predcessors } in
            updatedProgram 
            |> ParsedProgram.addMnemonic name (Output operation), (Some operation)

        | BasicPatterns.Call (objExprOpt, memberOrFunc, _typeArgs1, _typeArgs2, argsExprs) ->
            match objExprOpt with
            | Some _ ->
                "Object method calling" |> notSupported
            | None ->
                let processArgumentExpr = processFuncArgs parsedProgram predcessors memberOrFunc.CompiledName 
                
                let updatedProgram, (Reversed argNames) = 
                    argsExprs
                    |> List.mapi processArgumentExpr
                    |> List.fold (fun (program, names) (diff, mnemo) ->
                        program + diff, mnemo :: names) (parsedProgram, [])
                let name = 
                    match nameOpt with 
                    | Some str -> str 
                    | None -> ParsedProgram.escapeName updatedProgram (memberOrFunc.FullName + "Output")
                let dependencies = findDependencies updatedProgram argNames
                let operation = { name = memberOrFunc.FullName;
                                  inputs = argNames;
                                  output = name;
                                  parameters = 
                                      match _typeArgs2 with
                                      | [] -> None
                                      | _ -> Some (_typeArgs2 |> List.map typeToModel)
                                  dependencies = Set.union dependencies predcessors } in
                updatedProgram 
                |> ParsedProgram.addMnemonic name (Output operation), (Some operation)
                
        | BasicPatterns.Value value ->
            let updatedProgram = 
                match nameOpt with
                | Some name -> 
                    parsedProgram
                    |> ParsedProgram.addMnemonic name (Alias value.CompiledName)
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
            "Such expression" |> notSupported
    and processFuncArgs parsedProgram predcessors funcName i argExpr = 
        match argExpr with
        | BasicPatterns.Value value
        | BasicPatterns.Call (None, value, [], [], []) -> 
            ProgramDiff.empty, value.CompiledName
        | _ -> 
            let argName = ParsedProgram.escapeName parsedProgram (funcName + (string i))
            let updatedProgram, _ = visitExpression (Some argName) predcessors parsedProgram argExpr in
            (updatedProgram - parsedProgram), argName        

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
        
    let parse optSources source = 
        let targetName = tempFileName ()
        let checkResults = getProjectTypedTree targetName source optSources
        if checkResults.HasCriticalErrors then
            checkFailed checkResults.Errors
        else
            let targetFile = 
                checkResults.AssemblyContents.ImplementationFiles
                |> List.filter (fun file -> file.FileName = targetName)
                |> List.exactlyOne
            let errors = 
                checkResults.Errors
                |> Seq.filter (fun e -> e.FileName = targetName)
                |> Seq.map (fun e ->
                    { StartLine = e.StartLineAlternate
                      StartColumn = e.StartColumn
                      EndLine = e.EndLineAlternate
                      EndColumn = e.EndColumn
                      Message = e.Message
                      Subcategory = e.Subcategory
                      ErrorNumber = e.ErrorNumber
                      Severity = 
                          match e.Severity with 
                          | FSharpErrorSeverity.Warning -> ErrorSeverity.Warning
                          | FSharpErrorSeverity.Error -> ErrorSeverity.Error })
            let graph = 
                parseProgramTree targetFile
                |> ParsedProgram.toComputationGraph
            graph, errors

    let parseSingle = parse []