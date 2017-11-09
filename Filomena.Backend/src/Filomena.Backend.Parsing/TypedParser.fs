namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler.SourceCodeServices

open Filomena.Backend.Models
open Filomena.Backend.Parsing.ProjectHelper
open Filomena.Backend.Parsing.Maybe

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

    type ExpressionProduct = 
        | SourceOperation of int // indicates that expression results to some value produced by operation with index returned
        | Mnemonic of string // indicates that expression results to some known value identified by mnemonic name returned
        | Operation of string // indicates that expression results to sone known function identified by full entity name

    let uniqueName () = System.Guid.NewGuid () |> sprintf "%A"

    let rec visitExpression (name: string option) (parsedProgram: ParsedProgram) (expr: FSharpExpr) = 
        match expr with
        | BasicPatterns.Let ((bindVar, bindExpr), bodyExpr) ->
            Failed "Oops"
        | BasicPatterns.Call (objExpr, memberOrFunc, typeArgs1, typeArgs2, argsExprs) ->
            Failed "Oops"
        | BasicPatterns.Value (value) ->
            Failed "Oops"
        | BasicPatterns.Const (objVal, fsType) ->
            let dataType = typeToModel fsType
            let mnemoName = 
                match name with
                | Some str -> str
                | None -> ParsedProgram.escapeName parsedProgram (dataType.name + "val")
            in
            Ok (ParsedProgram.addMnemonic parsedProgram mnemoName (Const (string objVal, dataType))) // check if "string" serialization is correct
        | _ -> 
            Failed "Expression is not supported"

    let rec visitExpression' graph metaValues expr =
        let valueSources, duplicates = metaValues
        let rec escapeName graph metaValues name = 
            let valueSources, duplicates = metaValues
            let containsName map = Map.containsKey name map in
            if graph.mnemonicsTable |> containsName 
                || valueSources |> containsName 
                || duplicates |> containsName 
            then 
                escapeName graph metaValues (name + "'")
            else 
                name
        let bindExpressionName graph metaValues bindVar bindExpr = 
            maybe { let name = escapeName graph metaValues bindVar
                    let! newGraph, product = visitExpression' graph metaValues bindExpr in
                    return 
                        match product with
                        | SourceOperation code ->
                            let updatedSources = valueSources |> Map.add name code in
                            newGraph, (updatedSources, duplicates)
                        | Mnemonic value ->
                            let updatedDuplicates = duplicates |> Map.add name value in
                            newGraph, (valueSources, updatedDuplicates) } in
        match expr with
        | BasicPatterns.Sequential (firstExpr, secondExpr) -> 
            maybe { let! newGraph, firstProd = visitExpression' graph metaValues firstExpr
                    let! lastGraph, secondProd = visitExpression' newGraph metaValues secondExpr in
                    return
                        match firstProd, secondProd with
                        | SourceOperation firstOp, SourceOperation secondOp ->
                            if lastGraph.dependencies.[secondOp] |> Set.contains firstOp then
                                lastGraph, SourceOperation secondOp // TODO: check if opcode is correct
                            else
                                let newDependencies = 
                                    lastGraph.dependencies 
                                    |> List.mapi (fun i deps -> if i = secondOp then Set.add firstOp deps else deps) in
                                { operations = lastGraph.operations; 
                                  dependencies = newDependencies; 
                                  mnemonicsTable = lastGraph.mnemonicsTable }, SourceOperation secondOp // TODO: check if opcode is correct
                        | _ ->
                            lastGraph, secondProd }
        // TODO: ensure that operations cannot be used as arguments
        | BasicPatterns.Call (objExprOpt, memberOrFuc, typeArgs1, typeArgs2, argExprs) ->
            
            Failed "Oops"
                    
            // Failed "Not implemented" // TODO: implement
        | BasicPatterns.Application (funExpr, typeArgs, argExprs) ->

            Failed "Not implemented" // TODO: implement
        | BasicPatterns.Let ((bindVar, bindExpr), bodyExpr) -> 
            maybe { let! newGraph, newMetavalues = bindExpressionName graph metaValues bindVar.FullName bindExpr in            
                    return! visitExpression' newGraph newMetavalues bodyExpr }
        | BasicPatterns.Value (value) ->
            match duplicates |> Map.tryFind value.FullName with
            | Some mnemonic ->
                Ok (graph, Mnemonic mnemonic)
            | None ->
                Ok (graph, Mnemonic value.FullName)
        | BasicPatterns.Const (constValObj, constType) ->
            let mnemonicValue = { dataType = typeToModel constType; value = string constValObj } // TODO: check if custom ToString needed
            let mnemonic = uniqueName ()
            let updatedGraph = { operations = graph.operations;
                                 dependencies = graph.dependencies;
                                 mnemonicsTable = Map.add mnemonic mnemonicValue graph.mnemonicsTable } in
            Ok (updatedGraph, Mnemonic mnemonic)
        | BasicPatterns.NewArray (arrType, initlist) ->
            Failed "Not implemented" // TODO: implement
        | BasicPatterns.NewTuple (tupType, initList) ->
            Failed "Not implemented" // TODO: implement
        | BasicPatterns.TupleGet (tupType, elemIndex, tupleExpr) ->
            Failed "Not implemented" // TODO: implement
        | BasicPatterns.Lambda (lambdaVar, bodyExpr) ->
            Failed "Lambdas are currently unsupported"
        | BasicPatterns.IfThenElse (condExpr, thenExpr, elseExpr) ->
            Failed "Conditions are unsupported until lambda are ready"
        | BasicPatterns.TypeTest (suspectType, exprToTest) ->
            Failed "Type test is currently usupported"
        | BasicPatterns.DefaultValue (defaultType) ->
            Failed "Default values are not supported"
        | _ ->
            Failed "Construction is not supported"

    let (|Function|) (x: FSharpMemberOrFunctionOrValue) = x.FullType.IsFunctionType

    let visitDeclarations graph declaration = 
        match declaration with
        | Entity (entity, subDecls) ->
            Failed "Nested types or modules are not allowed"
        | InitAction (expr) ->
            // visitExpression graph expr
            Ok ()
        | MemberOrFunctionOrValue (funcOrVal, funcOrValss, expression) ->
            
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
        