namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler

type UntypedCheckError = UntypedCheckError of string * Range.range option

type ParseErrors = 
| UntypedCheckErrors of UntypedCheckError list 
| FSharpErrors of FSharpErrorInfo []

[<RequireQualifiedAccess>]
type ErrorSeverity = Warning | Error

type ParsingError = { ErrorNumber: int
                      StartLine: int
                      StartColumn: int
                      EndLine: int
                      EndColumn: int
                      Severity: ErrorSeverity
                      Subcategory: string
                      Message: string }
            

module ParsingError = 
    let ofFSharpErrorInfo (e: FSharpErrorInfo) = 
        { StartLine = e.StartLineAlternate
          StartColumn = e.StartColumn
          EndLine = e.EndLineAlternate
          EndColumn = e.EndColumn
          Message = e.Message
          Subcategory = e.Subcategory
          ErrorNumber = e.ErrorNumber
          Severity = 
              match e.Severity with 
              | FSharpErrorSeverity.Warning -> ErrorSeverity.Warning
              | FSharpErrorSeverity.Error -> ErrorSeverity.Error }

    let ofUntypedCheckError (e: UntypedCheckError) = 
        let (UntypedCheckError (msg, rangeOpt)) = e
        match rangeOpt with
        | Some range ->
            { StartLine = range.StartLine
              StartColumn = range.StartColumn
              EndLine = range.EndLine
              EndColumn = range.EndColumn
              Message = msg
              Subcategory = System.String.Empty
              ErrorNumber = -1
              Severity = ErrorSeverity.Error }
        | None ->
            { StartLine = 0
              StartColumn = 0
              EndLine = 0
              EndColumn = 0
              Message = msg
              Subcategory = System.String.Empty
              ErrorNumber = -1
              Severity = ErrorSeverity.Error }

    let ofFSharpErrorInfos (es: FSharpErrorInfo seq) = 
        Seq.map ofFSharpErrorInfo es

    let ofMsgAndRange (msg: string) (range: Range.range) = 
        { StartLine = range.StartLine
          StartColumn = range.StartColumn
          EndLine = range.EndLine
          EndColumn = range.EndColumn
          Message = msg
          Subcategory = System.String.Empty
          ErrorNumber = -1
          Severity = ErrorSeverity.Error }