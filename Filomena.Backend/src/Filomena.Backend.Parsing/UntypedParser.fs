namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open Filomena.Backend.Parsing.ProjectHelper

module UntypedParser =
    exception ParsingError of FSharpErrorInfo [] with
        static member failwith (errors: FSharpErrorInfo []) = 
            raise (ParsingError (errors))
            
    exception ChekingError of string * Range.range with
        static member failwith message range = 
            raise (ChekingError (message, range))
    
    module Ident = 
        let idText (ident: Ident) = ident.idText
        
        let toString (ident: LongIdentWithDots) = 
            ident.Lid
            |> List.map idText
            |> String.concat "."
            
    let checker = FSharpChecker.Create ()
    
    let defaultFileName = "#$DefaultFileName"
    let tab = "    "
    
    let getUntypedTreeFromProject fileName source = 
        async {
            let! projOptions, errors = checker.GetProjectOptionsFromScript (fileName, source) in
            if errors.Length = 0 then    
                let! parseFileResults = checker.ParseFileInProject (defaultFileName, source, projOptions) in
                return 
                    match parseFileResults.ParseTree with
                    | Some tree -> tree
                    | None -> ParsingError.failwith parseFileResults.Errors
            else
                return ParsingError.failwith (List.toArray errors)
        }
        |> Async.RunSynchronously
        
    let getUntypedTree source = getUntypedTreeFromProject (projectFromScript source) source
    
    let getUntypedTreeNoSettings source = getUntypedTreeFromProject (emptyProject ()) source
    
    let visitConst constRange = function 
        | SynConst.Bool (flag) -> string flag
        | SynConst.Byte (bt) -> string bt
        | SynConst.Bytes (bytes, _) -> 
            bytes 
            |> Array.map (fun x -> string x) 
            |> String.concat "; "
            |> sprintf "[|%s|]"
        | SynConst.Char (ch) -> string ch
        | SynConst.Decimal (d) -> string d
        | SynConst.Double (d) -> string d
        | SynConst.Int32 (i) -> string i
        | SynConst.Measure (mConst, measure) ->
            do ignore mConst
            do ignore measure
            ChekingError.failwith "Measures are currently unsupported" constRange
        | SynConst.SByte (sb) -> string sb
        | SynConst.Single (s) -> string s
        | SynConst.String (s, _) -> s |> sprintf "\"%s\""
        | SynConst.UInt32 (u) -> string u
        | SynConst.Unit -> "()"
        | _ -> 
            ChekingError.failwith "This constant is unsupported" constRange 
            
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
            ChekingError.failwith "Pattern is unsupported" range
        | SynPat.FromParseError (_, range) ->
            ChekingError.failwith "Parse error" range

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
                ChekingError.failwith "Parsing error" range
        | _ -> 
            // TODO: implement
            failwith "Not implemented"
        
    
        
       
    