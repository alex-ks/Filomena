namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open Filomena.Backend.Parsing.ProjectHelper

module UntypedParser =
    exception ParsingError of FSharpErrorInfo [] with
        static member failwith (errors: FSharpErrorInfo []) = 
            raise (ParsingError (errors))
            
    exception ChekingError of string * Range with
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
        | SynConst.Bytes (bytes, range) -> 
            bytes 
            |> Array.map (fun x -> string x) 
            |> String.concat "; "
            |> sprintf "[|%s|]"
        | SynConst.Char (ch) -> string ch
        | SynConst.Decimal (d) -> string d
        | SynConst.Double (d) -> string d
        | SynConst.Int32 (i) -> string i
        | SynConst.Measure (mConst, measure) -> 
            ChekingError.failwith "Measures are currently unsupported" constRange
        | SynConst.SByte (sb) -> string sb
        | SynConst.Single (s) -> string s
        | SynConst.String (s, range) -> s |> sprintf "\"%s\""
        | SynConst.UInt32 (u) -> string u
        | SynConst.Unit -> "()"
        | _ -> 
            ChekingError.failwith "This constant is unsupported" constRange 
        
       
    