namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open Filomena.Backend.Parsing // to hide ParserDetail union with Maybe union
open Filomena.Backend.Parsing.ProjectHelper
open Filomena.Backend.Parsing.UntypedParser
open Filomena.Backend.Parsing.Exceptions

module PartialParser =
    let visitMdDeclarations decls = 
        decls
        |> Seq.filter (function 
            | SynModuleDecl.Open _ -> true
            | _ -> false)
        |> Seq.map (function 
            | SynModuleDecl.Open (ident, _) ->
                Ident.toString ident
            | _ ->
                unexpected "Only open must be filtered here")
        |> Seq.toList        

    let visitModules modules = 
        let md = Seq.exactlyOne modules
        let (SynModuleOrNamespace(longIdent, isRecursive, isModule, moduleDecls, _, _, _, range)) = md
        let moduleName = Ident.listToString longIdent
        let errors = 
            let nameChecked = 
                if moduleName |> isTemp then
                    [ ParsingError.ofMsgAndRange ParsingResources.noModuleNameMsg range ]
                else []
            let recChecked = 
                if isRecursive then
                    (ParsingError.ofMsgAndRange ParsingResources.recursionIsNotAllowedMsg range)::nameChecked
                else
                    nameChecked
            let namespaceChecked = 
                if not isModule then
                    (ParsingError.ofMsgAndRange ParsingResources.namespaceIsNotAllowed range)::recChecked
                else
                    recChecked
            namespaceChecked
        if not (errors |> List.isEmpty) then
            checkFailed errors
        else
            moduleName, (visitMdDeclarations moduleDecls)
   
    let partialParse source = 
        match getUntypedTree source with
        | Ok tree -> 
            match tree with
            | ParsedInput.ImplFile file ->
                let (ParsedImplFileInput(_, _, _, _, _, modules, _)) = file in
                visitModules modules
            | ParsedInput.SigFile _ ->
                notSupported ParsingResources.signatureFilesAreNotAllowed
        | Failed errors -> 
            checkFailed (ParsingError.ofFSharpErrorInfos errors |> Seq.toList)