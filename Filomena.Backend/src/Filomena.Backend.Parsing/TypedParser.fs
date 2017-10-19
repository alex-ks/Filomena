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
        let name = t.TypeDefinition.CompiledName
        let parameters = 
            if t.GenericArguments.Count = 0 then None 
            else Some (t.GenericArguments |> Seq.map typeToModel |> Seq.toList) in
        { name = name; parameters = parameters }

    type ExpressionProduct = SourceOperation of int | Mnemonic of string

    let rec visitExpression graph metaValues expr =
        let valueSources, duplicates = metaValues
        match expr with
        | BasicPatterns.Sequential (firstExpr, secondExpr) -> 
            maybe {
                let! newGraph, firstProd = visitExpression graph metaValues firstExpr
                let! lastGraph, secondProd = visitExpression newGraph metaValues secondExpr in
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
                    | _, SourceOperation secondOp ->
                        lastGraph, SourceOperation secondOp
                    | _, Mnemonic secondVal ->
                        lastGraph, Mnemonic secondVal                
            }          
        | BasicPatterns.Call (objExprOpt, memberOrFuc, typeArgs1, typeArgs2, argExprs) ->
            Failed "Not implemented" // TODO: implement
        | BasicPatterns.Application (funExpr, typeArgs, argExprs) ->
            Failed "Not implemented" // TODO: implement
        | BasicPatterns.Let ((bindVar, bindExpr), bodyExpr) ->
            maybe {
                let! newGraph, product = visitExpression graph metaValues bindExpr
                return! 
                    match product with
                    | SourceOperation code ->
                        let updatedSources = valueSources |> Map.add bindVar.CompiledName code in
                        visitExpression graph (updatedSources, duplicates) bodyExpr
                    | Mnemonic value ->
                        let updatedDuplicates = Map.add bindVar.CompiledName value duplicates in
                        visitExpression graph (valueSources, duplicates) bodyExpr 
            }            
        | BasicPatterns.Value (value) ->
            match duplicates |> Map.tryFind value.CompiledName with
            | Some mnemonic ->
                Ok (graph, Mnemonic mnemonic)
            | None ->
                Ok (graph, Mnemonic value.CompiledName)
        | BasicPatterns.Const (constValObj, constType) ->
            let mnemonicValue = { dataType = typeToModel constType; value = string constValObj } // TODO: check if custom ToString needed
            let mnemonic = System.Guid.NewGuid () |> sprintf "#%A"
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
        | FSharpImplementationFileDeclaration.Entity (entity, subDecls) ->
            Failed "Nested types or modules are not allowed"
        | FSharpImplementationFileDeclaration.InitAction (expr) ->
            // visitExpression graph expr
            Ok ()
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
        