namespace Filomena.Backend.Parsing

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler

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
        

exception NotSupportedException of string

exception UnexpectedException of string

exception CheckException of ParsingError list

module Exceptions = 
    /// Raises System.NotImplementedException with message "{feature} not implemented"
    let notImplemented feature = raise (System.NotImplementedException (feature + " not implemented"))

    /// Raises NotSupportedException with message "{feature} not supported"
    let notSupported feature = raise (NotSupportedException (feature + " not supported"))

    /// Raises an UnexpectedException with specified message
    let unexpected message = raise (UnexpectedException message)

    /// Checks condition and raises UnexpectedException if condition is false
    let expect condition failMessage = if not condition then unexpected failMessage

    let checkFailed errors = raise (CheckException errors)
