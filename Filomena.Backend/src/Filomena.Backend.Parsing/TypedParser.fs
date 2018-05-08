namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler.SourceCodeServices

open ProjectHelper
open Exceptions
open System.Reflection.Metadata

type ModuleSource = WorkflowSource of string | DeclSource of string

module TypedParser =
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let defaultFileVersion = 0
    
    let getProjectOptions filesNames projName = 
        let compilerParams = 
            [| yield "--simpleresolution" 
               yield "--noframework" 
               yield "--debug:full" 
               yield "--define:DEBUG" 
               yield "--optimize-" 
               yield "--out:" + (changeToDll projName)
               yield "--doc:test.xml" 
               yield "--warn:3" 
               yield "--fullpaths" 
               yield "--flaterrors" 
               yield "--target:library"
               yield "--mlcompatibility"
               for name in filesNames -> 
                   name
               let references =
                 [ sysLib "mscorlib" 
                   sysLib "System"
                   sysLib "System.Core"
                   sysLib "System.Runtime"
                   sysLib "System.Private.CoreLib"
                   fscorePath ]
               for r in references -> "-r:" + r |]
        let projectName = changeToFsproj projName
        checker.GetProjectOptionsFromCommandLineArgs (projectName, compilerParams)

    let performTypedCheck declNames wfNames =
        let projectOptions = getProjectOptions (Seq.append declNames wfNames) (Seq.last wfNames)
        projectOptions
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    
    let dispose (x: System.IDisposable) = x.Dispose ()

    let checkSourcesTyped declSources wfSources = 
        let generateFilesFor sources = 
            sources
            |> Seq.map (fun code -> new TempFile(tempFileName (), code))
            |> Seq.toList
        let getNames (files: TempFile seq) = files |> Seq.map (fun f -> f.Name)        

        let declFiles = generateFilesFor declSources
        let wfFiles = generateFilesFor wfSources

        use _guard = { new System.IDisposable with
                       member __.Dispose () = for f in (Seq.append declFiles wfFiles) do dispose f }
        
        let wfNames = getNames wfFiles
        let declNames = getNames declFiles
        let checkResults = performTypedCheck declNames wfNames in
        if checkResults.HasCriticalErrors then
            Error checkResults.Errors
        else
            let fileTrees = 
                checkResults.AssemblyContents.ImplementationFiles
                |> List.map (fun f -> f.FileName, f)
                |> Map.ofList
            let wfTrees = 
                wfNames
                |> Seq.map (fun name -> fileTrees.[name])
            Ok (wfTrees, checkResults.Errors)            
    
    type DataType = Filomena.Backend.Models.DataType

    // TODO: handle measure types?
    let rec typeToModel (t: FSharpType) =
        if PrimitiveTypes.TypeMap |> Map.containsKey t.TypeDefinition.DisplayName then
            PrimitiveTypes.TypeMap.[t.TypeDefinition.DisplayName]
        else
            let name = 
                match t.TypeDefinition.TryFullName with
                | Some fullName -> fullName
                | None -> t.TypeDefinition.DisplayName
            
            let parameters = 
                match t.GenericArguments |> Seq.toList with
                | [] -> None             
                | _ -> Some (t.GenericArguments |> Seq.map typeToModel |> Seq.toList)
            in
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

    let operators = Set.ofList ["+"; "-"; "*"; "/"; "%"; "**"]

    let nameOperation (op: FSharpMemberOrFunctionOrValue) = 
        match op.DisplayName.Split(' ') with
        | [|"("; x; ")"|] -> 
            if operators |> Set.contains x then x else op.FullName
        | _ -> op.FullName

    type Argument = Expression of FSharpExpr | Constant of string * DataType

    let unnamedInput funcName = "@" + funcName + "Input"
    let unnamedOutput funcName = "@" + funcName + "Output"

    module Argument = 
        let ofExprs exprs = exprs |> List.map (Expression)
        let ofInt (i: int) = Constant (string i, PrimitiveTypes.Int)

    let rec visitExpression (nameOpt: string option) 
                            predcessors
                            (parsedProgram: ParsedProgram) 
                            (expr: FSharpExpr) =
        let visitCall (objExprOpt, 
                       fullFuncName,
                       funcTypeOpt,
                       _typeArgs1, 
                       typeArgs2, 
                       argsExprs) = 
            match objExprOpt with
            | Some _ ->
                "Object method calling" |> notSupported
            | None ->
                let processArgumentExpr = processFuncArgs parsedProgram predcessors fullFuncName
                let updatedProgram, (Reversed argNames) = 
                    argsExprs
                    |> List.mapi processArgumentExpr
                    |> List.fold (fun (program, names) (diff, mnemo) ->
                        program + diff, mnemo :: names) (parsedProgram, [])
                let name = 
                    match nameOpt with 
                    | Some str -> str 
                    | None -> ParsedProgram.escapeName updatedProgram (unnamedOutput fullFuncName)
                let dependencies = findDependencies updatedProgram argNames
                let operation = 
                    let template = 
                        { name = fullFuncName;
                          inputs = argNames;
                          output = name;
                          parameters = 
                              match typeArgs2 with
                              | [] -> None
                              | _ -> Some (List.map typeToModel typeArgs2)
                          dependencies = Set.union dependencies predcessors }
                    match argsExprs, funcTypeOpt with
                    | [], Some t ->
                        { template with name = HiddenOps.Identity;
                                        inputs = [ fullFuncName ];
                                        parameters = Some [ typeToModel t ] }
                    | [], None ->
                        unexpected "Renaming must provide value type"
                    | _, _ ->
                        template
                in 
                updatedProgram 
                |> ParsedProgram.addMnemonic name (Output operation), (Set.add operation predcessors)

        match expr with
        | BasicPatterns.Sequential (expr1, expr2) ->
            let updatedProgram, predcessors' = visitExpression None predcessors parsedProgram expr1
            visitExpression nameOpt predcessors' updatedProgram expr2
            
        | BasicPatterns.Let ((bindVar, bindExpr), bodyExpr) ->
            let updatedProgram, _ = visitExpression (Some bindVar.FullName) predcessors parsedProgram bindExpr
            visitExpression nameOpt predcessors updatedProgram bodyExpr

        | BasicPatterns.TupleGet (tupleType, index, tupleExpr) ->
            visitCall (None,
                       HiddenOps.TupleGet,
                       None,
                       [],
                       Seq.toList tupleType.GenericArguments,
                       [Argument.ofInt index; Expression tupleExpr])

        | BasicPatterns.NewTuple (tupleType, argsExprs) ->
            visitCall (None,
                       HiddenOps.NewTuple,
                       None,
                       [],
                       Seq.toList tupleType.GenericArguments,
                       Argument.ofExprs argsExprs)

        | BasicPatterns.NewUnionCase (_t, _case, _args) ->
            "Unions" |> notSupported

        | BasicPatterns.NewArray (t, argsExprs) ->
            visitCall (None,
                       HiddenOps.NewArray,
                       None,
                       [],
                       [ t ],
                       Argument.ofExprs argsExprs)

        | BasicPatterns.Call (objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argsExprs) ->
            visitCall (objExprOpt, 
                       nameOperation memberOrFunc, 
                       memberOrFunc.FullTypeSafe, 
                       typeArgs1, 
                       typeArgs2, 
                       Argument.ofExprs argsExprs)
                
        | BasicPatterns.Value value ->
            let updatedProgram = 
                match nameOpt with
                | Some name -> 
                    let dependencies = 
                        match parsedProgram.mnemonics.[value.FullName] with
                        | Const _ -> []
                        | Output op -> [op]
                        |> Set.ofList
                    let copyOp = { name = HiddenOps.Identity
                                   inputs = [value.FullName]
                                   output = name
                                   parameters = Some [typeToModel value.FullType]
                                   dependencies = dependencies }
                    parsedProgram
                    |> ParsedProgram.addMnemonic name (Output copyOp)
                | None ->
                    parsedProgram
            updatedProgram, predcessors

        | BasicPatterns.Const (objVal, fsType) ->
            let dataType = typeToModel fsType
            let name = 
                match nameOpt with
                | Some str -> str
                | None -> ParsedProgram.escapeName parsedProgram (dataType.name + "val")
            let updatedProgram = 
                parsedProgram
                |> ParsedProgram.addMnemonic name (Const (serializeConst objVal, dataType))
            updatedProgram, predcessors

        | _ ->
            printfn "%A" expr
            "Such expression" |> notSupported
    and processFuncArgs parsedProgram predcessors funcName i arg = 
        match arg with
        | Expression argExpr ->
            match argExpr with
            | BasicPatterns.Value value
            | BasicPatterns.Call (None, value, [], [], []) -> 
                ProgramDiff.empty, value.FullName
            | _ -> 
                let argName = ParsedProgram.escapeName parsedProgram ((unnamedInput funcName) + (string i))
                let updatedProgram, _ = visitExpression (Some argName) predcessors parsedProgram argExpr in
                (updatedProgram - parsedProgram), argName
        | Constant (value, t) ->
            let argName = ParsedProgram.escapeName parsedProgram (funcName + (string i))
            let updatedProgram = ParsedProgram.addMnemonic argName (Const (value, t)) parsedProgram
            (updatedProgram - parsedProgram), argName

    let (|Function|) (x: FSharpMemberOrFunctionOrValue) = x.FullType.IsFunctionType

    let rec visitDeclarations program predcessors declarations = 
        match declarations with
        | [] ->
            program, predcessors
        | decl :: tail ->
            match decl with
            | Entity _ ->
                failwith "Nested types or modules are not allowed"

            | InitAction expr ->
                let updatedProgram, predcessors' = visitExpression None predcessors program expr
                visitDeclarations updatedProgram predcessors' tail

            | MemberOrFunctionOrValue (funcOrVal, _, expression) ->
                if (|Function|) funcOrVal then
                    "Functions declaration" |> notSupported
                else
                    let updatedProgram, predcessors' = 
                        visitExpression (Some funcOrVal.FullName) predcessors program expression
                    visitDeclarations updatedProgram predcessors' tail
            
    let (|Module|) (x: FSharpEntity) = if x.IsFSharpModule then Some x else None
    
    let parseProgramTree (implFiles: FSharpImplementationFileContents seq) =
        let program, _ = 
            implFiles
            |> Seq.fold (fun (program, predcessors) implFile ->
                            match implFile.Declarations with
                            | [Entity(Module _, subdecls)] ->
                                do printfn "%A" subdecls
                                visitDeclarations program predcessors subdecls
                            | [_] -> 
                                failwith "Top-level declaration can be module only"
                            | [] ->
                                failwith "Empty file"
                            | _ ->
                                failwith "Too many declarations")
                        (ParsedProgram.empty, Set.empty)
        in program        

    let parse mdSources = 
        let declSources = 
            mdSources
            |> Seq.fold (fun wfs -> function | DeclSource source -> source :: wfs
                                             | WorkflowSource _ -> wfs) 
                        []
            |> Seq.rev
        let wfSources = 
            mdSources
            |> Seq.fold (fun wfs -> function | WorkflowSource source -> source :: wfs
                                             | DeclSource _ -> wfs) 
                        []
            |> Seq.rev

        let checkResults = checkSourcesTyped declSources wfSources
        match checkResults with
        | Error errors ->
            errors
            |> ParsingError.ofFSharpErrorInfos 
            |> Seq.toList
            |> checkFailed 
        | Ok (trees, checkWarnings) ->
            let errors = ParsingError.ofFSharpErrorInfos checkWarnings
            let graph = 
                trees
                |> parseProgramTree
                |> ParsedProgram.toComputationGraph
            graph, errors
