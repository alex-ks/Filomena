namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open Filomena.Backend.Parsing // to hide ParserDetail union with Maybe union
open Filomena.Backend.Parsing.ProjectHelper

module UntypedParser =
    type CheckError = CheckError of string * Range.range option

    type ParseErrors = 
    | CheckErrors of CheckError list 
    | FSharpErrors of FSharpErrorInfo []

    let checkReduce accum next = 
        match next with
        | Ok () -> accum
        | Failed errors -> 
            match accum with
            | Ok () -> Failed errors
            | Failed accumErrors -> Failed (accumErrors @ errors)

    let gatherReduce accum next =
        match next with
        | Ok modulesList ->
            match accum with
            | Ok accumList -> Ok (accumList @ modulesList)
            | Failed errorsList -> Failed errorsList
        | Failed errorsList -> 
            match accum with
            | Ok _ -> Failed errorsList
            | Failed accumErrors -> Failed (accumErrors @ errorsList)
    
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
        let parseFileResults = checker.ParseFileInProject (fileName, source, projectOptions) |> Async.RunSynchronously        
        match parseFileResults.ParseTree with
        | Some tree -> Ok tree
        | None -> Failed parseFileResults.Errors
        

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
        | SynConst.Unit ->
            Ok ()

        | SynConst.Measure _ ->
            Failed [CheckError ("Measures are currently unsupported", Some constRange)]

        | _ -> 
            Failed [CheckError ("This constant is unsupported", Some constRange)]

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
            Failed [CheckError ("Pattern is unsupported", Some range)]
            
        | SynPat.FromParseError (_, range) ->
            Failed [CheckError ("Parse error", Some range)]

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
                Failed [CheckError ("Parsing error", Some range)]
                
        | SynExpr.LetOrUse (isRec, isUse, bindings, body, wholeRange) ->
            if isRec then Failed [CheckError ("Recursion is not allowed", Some wholeRange)]
            elif isUse then Failed [CheckError ("Using construct is not allowed", Some wholeRange)]
            else
                [ visitBindings bindings
                  visitExpression body ] 
                |> List.reduce checkReduce

        | SynExpr.Lambda (_isFromMethod, _isLaterPart, _simplePatterns, _body, range) ->
            Failed [CheckError ("Lambdas are currently unsupported", Some range)]

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
            Failed [CheckError ("Match is currently unsupported", Some range)]

        | SynExpr.FromParseError (_, range) ->
            Failed [CheckError ("Parse error", Some range)]

        | _ -> 
            // TODO: implement
            Failed [CheckError ("Not implemented", None)]
    
    and visitBindings bindings = 
        let visitBinding binding = 
            let (Binding (_, kind, _, isMutable, _, _, _, pattern, _, body, range, _)) = binding in
            match kind with
            | SynBindingKind.DoBinding ->
                Failed [CheckError ("Such 'do' usage is not allowed", Some range)]

            | SynBindingKind.NormalBinding ->
                if not isMutable then
                    [ visitPattern pattern
                      visitExpression body ]
                    |> List.reduce checkReduce
                else
                    Failed [CheckError ("Mutable values are not alowed", Some range)]

            | SynBindingKind.StandaloneExpression ->
                Failed [CheckError ("Standalone expressions are not allowed", Some range)]

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
                    | Failed lst -> Failed lst
                else
                    Failed [CheckError ("Recursion is not allowed", Some range)]

            | SynModuleDecl.DoExpr (_sequencePointInfo, expression, _) ->
                match visitExpression expression with
                | Ok () -> Ok []
                | Failed lst -> Failed lst

            | SynModuleDecl.Open (ident, _) ->
                Ok [Ident.toString ident]

            | _ ->
                // TODO: handle all cases
                Failed [CheckError ("Not implemented", None)])
        |> Seq.reduce gatherReduce

    let visitModulesAndNamespaces modulesOrNss = 
        modulesOrNss
        |> Seq.map (fun moduleOrNs ->
            let (SynModuleOrNamespace(longIdent, isRecursive, isModule, moduleDecls, _, _, _, range)) = moduleOrNs
            let moduleName = Ident.listToString longIdent
            if moduleName |> isTemp then 
                Failed [CheckError ("Module name must be defined", Some range)]
            elif isRecursive then 
                Failed [CheckError ("Recursion is not allowed", Some range)]
            elif not isModule then 
                Failed [CheckError ("Namespace declaration is not allowed", Some range)]
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
                | Failed errorsList -> Failed (CheckErrors errorsList)
            | ParsedInput.SigFile _ ->
                Failed (CheckErrors [CheckError ("Interface files are not supported", None)])
        | Failed errors -> 
            Failed (FSharpErrors errors)
