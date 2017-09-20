namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open Filomena.Backend.Parsing.ProjectHelper

module UntypedParser =
    exception ParsingException of FSharpErrorInfo [] with
        static member failwith (errors: FSharpErrorInfo []) = 
            raise (ParsingException (errors))
            
    exception CheckingException of string * Range.range with
        static member failwith message range = 
            raise (CheckingException (message, range))
    
    module Ident = 
        let idText (ident: Ident) = ident.idText
        
        let toString (ident: LongIdentWithDots) = 
            ident.Lid
            |> List.map idText
            |> String.concat "."

        let listToString = List.map idText >> String.concat "."
            
    let checker = FSharpChecker.Create ()
    
    let tab = "    "

    let rec typeToString = function
        | SynType.LongIdent (ident) -> Ident.toString ident  
        | SynType.Array (n, elementType, range) -> typeToString elementType + "[]"
        | SynType.Fun (argType, returnType, range) -> typeToString argType + "->" + typeToString returnType
        | SynType.Tuple (typeNames, range) -> typeNames |> List.map (snd >> typeToString) |> String.concat " * "
        | SynType.Var (Typar(genericName, staticReq, isComplGenerated), range) -> genericName.idText // Generic type placeholder
        | _ -> failwith "Not supported"
    
    let getUntypedTreeFromProject fileName source = 
        
        let projOptions, errors = checker.GetProjectOptionsFromScript (fileName, source) |> Async.RunSynchronously in
        if errors.Length = 0 then    
            let parseFileResults = checker.ParseFileInProject (fileName, source, projOptions) |> Async.RunSynchronously in
            match parseFileResults.ParseTree with
            | Some tree -> tree
            | None -> ParsingException.failwith parseFileResults.Errors
        else
            ParsingException.failwith (List.toArray errors)
        
        
    let getUntypedTree source = getUntypedTreeFromProject (projectFromScript source) source
    
    let getUntypedTreeNoSettings source = getUntypedTreeFromProject (emptyProject ()) source
    
    let visitConst constRange = function 
        | SynConst.Bool (flag) -> string flag
        | SynConst.Byte (bt) -> string bt
        | SynConst.Bytes (bytes, _) -> 
            bytes 
            |> Array.map string
            |> String.concat "; "
            |> sprintf "[|%s|]"
        | SynConst.Char (ch) -> string ch
        | SynConst.Decimal (d) -> string d
        | SynConst.Double (d) -> string d
        | SynConst.Int32 (i) -> string i
        | SynConst.Measure (mConst, measure) ->
            do ignore mConst
            do ignore measure
            CheckingException.failwith "Measures are currently unsupported" constRange
        | SynConst.SByte (sb) -> string sb
        | SynConst.Single (s) -> string s
        | SynConst.String (s, _) -> s |> sprintf "\"%s\""
        | SynConst.UInt32 (u) -> string u
        | SynConst.Unit -> "()"
        | _ -> 
            CheckingException.failwith "This constant is unsupported" constRange 
            
    let rec visitPattern = function
        | SynPat.LongIdent (lid, _, _, _, _, _) -> Ident.toString lid
        | SynPat.Named (synPat, ident, isSelfIdentifier, _, _) ->
            do ignore isSelfIdentifier
            match synPat with
            | SynPat.Wild (_) -> Ident.idText ident
            | _ -> sprintf "%s as %s" (visitPattern synPat) (Ident.idText ident)
        | SynPat.Wild (_) -> "_"
        | SynPat.Tuple (pats, _) -> 
            pats 
            |> List.map visitPattern
            |> String.concat ", "
        | SynPat.Paren (pat, _) -> 
            visitPattern pat
            |> sprintf "(%s)"
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
            CheckingException.failwith "Pattern is unsupported" range
        | SynPat.FromParseError (_, range) ->
            CheckingException.failwith "Parse error" range

    let rec visitExpression = function
        | SynExpr.Const (c, range) -> c |> visitConst range
        | SynExpr.IfThenElse (cond, trueBranch, falseBranchOpt, _, isFromErrorRecovery, _, range) ->
            if not isFromErrorRecovery then
                match falseBranchOpt with
                | Some falseBranch ->
                    [ "if"
                      visitExpression cond
                      "then"
                      visitExpression trueBranch
                      "else"
                      visitExpression falseBranch ] 
                | None ->
                    [ "if"
                      visitExpression cond
                      "then"
                      visitExpression trueBranch ]
                |> String.concat " "
            else
                CheckingException.failwith "Parsing error" range
        | SynExpr.LetOrUse (isRec, isUse, bindings, body, wholeRange) ->
            if isRec then CheckingException.failwith "Recursion is not allowed" wholeRange
            elif isUse then CheckingException.failwith "Using construct is not allowed" wholeRange
            else
                [ "let"
                  visitBindings bindings
                  "in"
                  visitExpression body ] 
                |> String.concat " "
        | SynExpr.Lambda (_isFromMethod, _isLaterPart, _simplePatterns, _body, range) ->
            CheckingException.failwith "Lambdas are currently unsupported" range
        | SynExpr.App (_, isInfix, funcExpr, argExpr, _) ->
            if isInfix then 
                [visitExpression argExpr; visitExpression funcExpr]
            else
                [visitExpression funcExpr; visitExpression argExpr]
            |> String.concat " "
        | SynExpr.ArrayOrList (isList, exprs, _) ->
            List.map visitExpression exprs |> String.concat "; "
            |> if isList then sprintf "[ %s ]" else sprintf "[| %s |]"
        | SynExpr.Tuple (exprs, _, _) ->
            exprs
            |> List.map visitExpression
            |> String.concat ", "
        | SynExpr.Typed (expr, typeName, _) ->
            [ visitExpression expr
              ": "
              typeToString typeName ]
            |> String.concat ""
        | SynExpr.Paren (expr, _, _, _) ->
            visitExpression expr |> sprintf "(%s)"
        | SynExpr.LongIdent (_, ident, _, _) ->
            Ident.toString ident
        | SynExpr.Ident (ident) ->
            Ident.idText ident
        | SynExpr.Do (expr, range) ->
            visitExpression expr 
        | SynExpr.Match (_seqPoint, _expr, _matchClauses, _isExnMatch, range) ->
            CheckingException.failwith "Match is currently unsupported" range
        | SynExpr.FromParseError (_, range) ->
            CheckingException.failwith "Parse error" range
        | _ -> 
            // TODO: implement
            failwith "Not implemented"
    
    and visitBindings bindings = 
        let visitBinding binding = 
            let (Binding (_, kind, _, isMutable, _, _, _, pattern, retInfo, body, range, _)) = binding in
            match kind with
            | SynBindingKind.DoBinding ->
                CheckingException.failwith "Such 'do' usage is not allowed" range
            | SynBindingKind.NormalBinding ->
                if not isMutable then
                    [ visitPattern pattern
                      (match retInfo with
                       | Some (SynBindingReturnInfo(synType, _, _)) -> ": " + typeToString synType
                       | None -> "") 
                      "="
                      visitExpression body ]
                    |> String.concat " "
                else
                    CheckingException.failwith "Mutable values are not alowed" range
            | SynBindingKind.StandaloneExpression ->
                CheckingException.failwith "Standalone expressions are not allowed" range
        bindings |> Seq.map visitBinding |> String.concat " and "

    let visitDeclarations platform decls = 
        decls
        |> Seq.map (function
            | SynModuleDecl.Let (isRec, bindings, range) ->
                if not isRec then
                    [ "let"; visitBindings bindings ] |> String.concat " "
                else
                    CheckingException.failwith "Recursion is not allowed" range
            | SynModuleDecl.DoExpr (_sequencePointInfo, expression, _) ->
                [ "do"; visitExpression expression] |> String.concat " "
            | SynModuleDecl.Open (ident, range) ->
                try
                    Ident.toString ident
                    |> Platform.getModuleDefinition platform
                with
                | _ -> CheckingException.failwith "Unknown module" range
            | _ ->
                // TODO: handle all cases
                failwith "Not implemented")
        |> String.concat System.Environment.NewLine

    let visitModulesAndNamespaces platform modulesOrNss = 
        do ignore<Platform> platform
        modulesOrNss
        |> Seq.map (fun moduleOrNs ->
            let (SynModuleOrNamespace(longIdent, isRecursive, isModule, moduleDecls, _, _, _, range)) = moduleOrNs
            let moduleName = Ident.listToString longIdent
            if moduleName |> isTemp then 
                CheckingException.failwith "Module name must be defined" range
            elif isRecursive then 
                CheckingException.failwith "Recursion is not allowed" range
            elif not isModule then 
                CheckingException.failwith "Namespace declaration is not allowed" range
            else 
                visitDeclarations platform moduleDecls)
        |> String.concat System.Environment.NewLine

    let parseAndCheckScript platform source = 
        let tree = getUntypedTreeNoSettings source in
        match tree with
        | ParsedInput.ImplFile (file) ->
            let (ParsedImplFileInput(_, _, _, _, _, modules, _)) = file in
            visitModulesAndNamespaces platform modules
        | ParsedInput.SigFile _ ->
            failwith "Interface files are not supported"



        
    
        
       
    