namespace Filomena.Backend.Parsing

exception NotSupportedException of string

exception UnexpectedException of string

module Exceptions = 
    let notImplemented feature = raise (System.NotImplementedException (feature + " not implemented"))

    let notSupported feature = raise (NotSupportedException (feature + " not supported"))

    let unexpected message = raise (UnexpectedException message)

    let expect condition failMessage = if not condition then unexpected failMessage