namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open Filomena.Backend.Parsing.ProjectHelper

open FSharp.Core // to hide ParserDetail from FSharp.Compiler.Ast

module UntypedParser =
    let checkReduce accum next = 
        match next with
        | Ok () -> accum
        | Error errors -> 
            match accum with
            | Ok () -> Error errors
            | Error accumErrors -> Error (accumErrors @ errors)

    let gatherReduce accum next =
        match next with
        | Ok modulesList ->
            match accum with
            | Ok accumList -> Ok (accumList @ modulesList)
            | Error errorsList -> Error errorsList
        | Error errorsList -> 
            match accum with
            | Ok _ -> Error errorsList
            | Error accumErrors -> Error (accumErrors @ errorsList)
    
    module Ident = 
        let idText (ident: Ident) = ident.idText
        
        let toString (ident: LongIdentWithDots) = 
            ident.Lid
            |> List.map idText
            |> String.concat "."

        let listToString = List.map idText >> String.concat "."
            
    let checker = FSharpChecker.Create ()
    
    let getUntypedTreeFromProject fileName source = 
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
               yield fileName
               let references =
                 [ sysLib "mscorlib" 
                   sysLib "System"
                   sysLib "System.Core"
                   sysLib "System.Runtime"
                   sysLib "System.Private.CoreLib"
                   fscorePath ]
               for r in references do 
                     yield "-r:" + r |]
        let projectName = changeToFsproj fileName
        let projectOptions = checker.GetProjectOptionsFromCommandLineArgs (projectName, compilerParams)
        let parsingOptions, errors = checker.GetParsingOptionsFromProjectOptions projectOptions
        match errors with 
        | [] ->
            let parseFileResults = checker.ParseFile (fileName, source, parsingOptions) |> Async.RunSynchronously  
            match parseFileResults.ParseTree with
            | Some tree -> Ok tree
            | None -> Error parseFileResults.Errors
        | _ ->
            Error (List.toArray errors)
        

    let getUntypedTree source = 
        use file = new TempFile (tempFileName (), source)
        getUntypedTreeFromProject (file.Name) source
    
    
    let visitConst constRange = function 
        | SynConst.Bool _
        | SynConst.Byte _
        | SynConst.Bytes _
        | SynConst.Char _
        | SynConst.Decimal _
        | SynConst.Double _
        | SynConst.Int32 _
        | SynConst.SByte _
        | SynConst.Single _
        | SynConst.String _
        | SynConst.UInt32 _
        | SynConst.Measure _
        | SynConst.Unit ->
            Ok ()

        | _ -> 
            Error [UntypedCheckError (ParsingResources.constantUnsupportedMsg, Some constRange)]

    let rec visitPattern = function
        | SynPat.LongIdent _ -> Ok ()

        | SynPat.Named (synPat, _, _, _, _) -> visitPattern synPat

        | SynPat.Wild (_) -> Ok ()

        | SynPat.Tuple (pats, _) -> 
            pats 
            |> List.map visitPattern
            |> List.reduce checkReduce

        | SynPat.Paren (pat, _) -> visitPattern pat

        | SynPat.Const (synConst, range) -> visitConst range synConst
        
        // Unsupported
        | SynPat.Ands (_, range)
        | SynPat.Attrib (_, _, range)
        | SynPat.DeprecatedCharRange (_, _, range)
        | SynPat.InstanceMember (_, _, _, _, range)
        | SynPat.IsInst (_, range) // x :? int pattern in matching
        | SynPat.Null (range)
        | SynPat.OptionalVal (_, range) // for optional function arguments - let f x ?y
        | SynPat.Or (_, _, range)
        | SynPat.QuoteExpr (_, range)
        | SynPat.Record (_, range)
        | SynPat.StructTuple (_, range)
        | SynPat.Typed (_, _, range)
        | SynPat.ArrayOrList (_, _, range) ->
            Error [UntypedCheckError (ParsingResources.patternUnsupportedMsg, Some range)]
            
        | SynPat.FromParseError (_, range) ->
            Error [UntypedCheckError (ParsingResources.parseErrorMsg, Some range)]

    let rec visitExpression = function
        | SynExpr.Const (c, range) -> visitConst range c

        | SynExpr.IfThenElse (cond, trueBranch, falseBranchOpt, _, isFromErrorRecovery, _, range) ->
            if not isFromErrorRecovery then
                match falseBranchOpt with
                | Some falseBranch ->
                    [ visitExpression cond
                      visitExpression trueBranch
                      visitExpression falseBranch ]
                | None ->
                    [ visitExpression cond
                      visitExpression trueBranch ]
                |> List.reduce checkReduce
            else
                Error [UntypedCheckError (ParsingResources.parseErrorMsg, Some range)]
                
        | SynExpr.LetOrUse (isRec, isUse, bindings, body, wholeRange) ->
            if isRec then Error [UntypedCheckError (ParsingResources.recursionIsNotAllowedMsg, Some wholeRange)]
            elif isUse then Error [UntypedCheckError (ParsingResources.usingIsNotAllowedMsg, Some wholeRange)]
            else
                [ visitBindings bindings
                  visitExpression body ] 
                |> List.reduce checkReduce

        | SynExpr.Lambda (_isFromMethod, _isLaterPart, _simplePatterns, _body, range) ->
            Error [UntypedCheckError (ParsingResources.lambdasAreUnsupportedMsg, Some range)]

        | SynExpr.App (_, isInfix, funcExpr, argExpr, _) ->
            if isInfix then 
                [visitExpression argExpr; visitExpression funcExpr]
            else
                [visitExpression funcExpr; visitExpression argExpr]
            |> List.reduce checkReduce

        | SynExpr.ArrayOrList (_, exprs, _) ->
            exprs
            |> List.map visitExpression
            |> List.reduce checkReduce

        | SynExpr.Tuple (exprs, _, _) ->
            exprs
            |> List.map visitExpression
            |> List.reduce checkReduce

        | SynExpr.Typed (expr, _, _)
        | SynExpr.Paren (expr, _, _, _) ->
            visitExpression expr

        | SynExpr.LongIdent _
        | SynExpr.Ident _ ->
            Ok ()

        | SynExpr.Do (expr, _) ->
            visitExpression expr 

        | SynExpr.Match (_seqPoint, _expr, _matchClauses, _isExnMatch, range) ->
            Error [UntypedCheckError (ParsingResources.matchIsUnsupportedMsg, Some range)]

        | SynExpr.FromParseError (_, range) ->
            Error [UntypedCheckError (ParsingResources.parseErrorMsg, Some range)]

        | _ -> 
            // TODO: implement
            Error [UntypedCheckError (ParsingResources.notImplementedMsg, None)]
    
    and visitBindings bindings = 
        let visitBinding binding = 
            let (Binding (_, kind, _, isMutable, _, _, _, pattern, _, body, range, _)) = binding in
            match kind with
            | SynBindingKind.DoBinding ->
                Error [UntypedCheckError (ParsingResources.suchDoUsageNotAllowedMsg, Some range)]

            | SynBindingKind.NormalBinding ->
                if not isMutable then
                    [ visitPattern pattern
                      visitExpression body ]
                    |> List.reduce checkReduce
                else
                    Error [UntypedCheckError (ParsingResources.mutableIsNotAllowedMsg, Some range)]

            | SynBindingKind.StandaloneExpression ->
                Error [UntypedCheckError (ParsingResources.standaloneExprIsNotAllowed, Some range)]

        bindings 
        |> Seq.map visitBinding 
        |> Seq.reduce checkReduce

    let visitDeclarations decls = 
        decls
        |> Seq.map (function
            | SynModuleDecl.Let (isRec, bindings, range) ->
                if not isRec then
                    match visitBindings bindings with
                    | Ok () -> Ok []
                    | Error lst -> Error lst
                else
                    Error [UntypedCheckError (ParsingResources.recursionIsNotAllowedMsg, Some range)]

            | SynModuleDecl.DoExpr (_sequencePointInfo, expression, _) ->
                match visitExpression expression with
                | Ok () -> Ok []
                | Error lst -> Error lst

            | SynModuleDecl.Open (ident, _) ->
                Ok [Ident.toString ident]

            | SynModuleDecl.NestedModule _ ->
                Error [UntypedCheckError (ParsingResources.nestedModulesNotAllowedMsg, None)]

            | _ ->
                // TODO: handle all cases
                Error [UntypedCheckError (ParsingResources.notImplementedMsg, None)])
        |> Seq.reduce gatherReduce

    let visitModulesAndNamespaces modulesOrNss = 
        modulesOrNss
        |> Seq.map (fun moduleOrNs ->
            let (SynModuleOrNamespace(longIdent, isRecursive, isModule, moduleDecls, _, _, _, range)) = moduleOrNs
            let moduleName = Ident.listToString longIdent
            if moduleName |> isTemp then 
                Error [UntypedCheckError (ParsingResources.noModuleNameMsg, Some range)]
            elif isRecursive then 
                Error [UntypedCheckError (ParsingResources.recursionIsNotAllowedMsg, Some range)]
            elif not isModule then 
                Error [UntypedCheckError (ParsingResources.namespaceIsNotAllowed, Some range)]
            else 
                visitDeclarations moduleDecls)
        |> Seq.reduce gatherReduce

    let parseAndCheckScript source = 
        let maybeTree = getUntypedTree source in
        match maybeTree with
        | Ok tree -> 
            match tree with
            | ParsedInput.ImplFile (file) ->
                let (ParsedImplFileInput(_, _, _, _, _, modules, _)) = file in
                match visitModulesAndNamespaces modules with
                | Ok modulesList -> Ok modulesList
                | Error errorsList -> Error (UntypedCheckErrors errorsList)
            | ParsedInput.SigFile _ ->
                Error (UntypedCheckErrors [UntypedCheckError (ParsingResources.signatureFilesAreNotAllowed, None)])
        | Error errors -> 
            Error (FSharpErrors errors)
