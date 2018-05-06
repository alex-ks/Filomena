namespace Filomena.Backend.Parsing

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
