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

    let nothing = ()

    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    
    let infixCodes = dict [ "op_Division", "/"
                            "op_Subtraction", "-"
                            "op_Multiply", "*"
                            "op_LessThanOrEqual", "<="
                            "op_PipeRight", "|>" ]

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

    let rec typeToString = function
        | SynType.LongIdent (ident) -> identToString ident  
        | SynType.Array (n, elementType, range) -> typeToString elementType + "[]"
        | SynType.Fun (argType, returnType, range) -> typeToString argType + "->" + typeToString returnType
        | SynType.Tuple (typeNames, range) -> typeNames |> List.map (fun x -> snd x) |> List.map typeToString |> String.concat " * "
        | SynType.Var (Typar(genericName, staticReq, isComplGenerated), range) -> genericName.idText // Generic type placeholder
        | _ -> "unknown"

    let visitConst c =
        do match c with
            | SynConst.Bool (flag) ->
                printf "%b" flag
            | SynConst.Byte (bt) ->
                printf "%i" bt
            | SynConst.Bytes (bytes, range) ->
                printf """ "bytes string"b """
            | SynConst.Char (ch) ->
                printf "%c" ch
            | SynConst.Int32 (n) ->
                printf "%d" n
            | SynConst.Unit ->
                printf "()"
            | _ -> 
                printf "unsupported"
        Succeeded ()

    let rec visitPattern = function
        | SynPat.LongIdent (lid, idOpt, typarDelcsOpt, args, accessOpt, range) -> 
            do printf "%s" (identToString lid)
        | SynPat.Named (synPat, ident, isSelfIdentifier, accessOpt, range) ->
            match synPat with
            | SynPat.Wild (_) ->
                do printf "%s" ident.idText
            | _ -> 
                do visitPattern synPat
                do printf " as %s" ident.idText
        | SynPat.Wild (range) ->
            do printf "_"
        | SynPat.Tuple (pats, range) ->
            printf "("
            for pat in pats do 
                do visitPattern pat 
                do printf ", "
            printf ")"
        | _ -> do printf "unsupported"
    
    let rec visitExpression = function
        | SynExpr.Const (c, range) ->
            visitConst c         
        | SynExpr.IfThenElse (cond, trueBranch, falseBranchOpt, seqPointIfToThen, isFromErrorRecovery, ifToThenRange, ifToEndRange) ->
            do printf "if "
            do visitExpression cond |> ignore
            do printf " then "
            do visitExpression trueBranch |> ignore
            match falseBranchOpt with
            | Some expr ->
                do printf " else "
                do visitExpression expr |> ignore
            | None -> do nothing
            Succeeded ()
        | SynExpr.LetOrUse (isRec, isUse, bindings, body, wholeRange) ->
            do printf (if isUse then "use " else "let ")
            do visitBindings bindings
            visitExpression body
        | SynExpr.Lambda (isFromMethod, isLaterPart, simplePatterns, body, range) -> Succeeded ()
        | SynExpr.App (isAtomic, isInfix, funcExpr, argExpr, range) ->
            if isInfix then
                do visitExpression argExpr |> ignore
                do printf " "
                do visitExpression funcExpr |> ignore
            else
                do visitExpression funcExpr |> ignore
                do printf " "
                do visitExpression argExpr |> ignore
            Succeeded () // function call (application)
        | SynExpr.ArrayOrList (isList, exprs, range) -> Succeeded ()
        | SynExpr.Tuple (exprs, commaRanges, range) -> 
            printf "("
            for expr in exprs do 
                visitExpression expr |> ignore
                printf ", "
            printf ")"
            Succeeded ()
        | SynExpr.Typed (expr, typeName, range) ->
            do visitExpression expr |> ignore
            do printf ": %s" (typeToString typeName)
            Succeeded ()
        | SynExpr.Record (baseOpt, copyInfoOpt, recordFields, range) -> Succeeded ()
        | SynExpr.Paren (expr, leftRange, rightRangeOpt, wholeRange) ->
            do printf "("
            do visitExpression expr |> ignore
            do printf ")"
            Succeeded () // (expr)
        | SynExpr.Match (seqPoint, expr, matchClauses, isExnMatch, range) -> Succeeded () // (match expr with pat1 -> expr1 | ... | patN -> exprN)
        | SynExpr.LongIdent (isOptional, ident, altNameRefCell, range) -> 
            do printf "%s" (identToString ident)
            Succeeded ()
        | SynExpr.Ident (ident) ->
            let text = if infixCodes.ContainsKey ident.idText then infixCodes.[ident.idText] else ident.idText
            do printf "%s" text
            Succeeded () // Same as LongIdent (false, ident, None, ident.Range)
        | SynExpr.FromParseError (expr, range) -> Succeeded () // For error recovery
        | _ -> FailedWith "Unsupported expression"
    and 
        visitBindings bindings = 
        for binding in bindings do
            let (Binding (access, kind, isInline, isMutable, attrs, xmlDoc, data, pattern, retInfo, body, range, sequencePointInfo)) = binding
            do visitPattern pattern
            match retInfo with
            | Some (SynBindingReturnInfo(synType, range, attrs)) ->
                printf " : %s" (typeToString synType)
            | None -> ()
            do printf " = "
            do visitExpression body |> ignore
            printfn ""
    
    
    let visitDeclarations decls = 
        for decl in decls do 
            match decl with 
            | SynModuleDecl.Let(isRecursive, bindings, range) ->
                do printf "let "
                if isRecursive then do printf "rec "
                do visitBindings bindings
            | SynModuleDecl.Open(longIdentWithDots, range) ->
                do printfn "open %s" (identToString longIdentWithDots)
                
            | SynModuleDecl.DoExpr(sequencePointInfo, expression, range) ->
                do visitExpression expression |> ignore
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
