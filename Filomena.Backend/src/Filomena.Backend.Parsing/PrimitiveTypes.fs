namespace Filomena.Backend.Parsing

open Filomena.Backend.Models

[<RequireQualifiedAccess>]
module PrimitiveTypes = 
    let Int = { name = "int"; parameters = None }
    let Float = { name = "float"; parameters = None }
    let String = { name = "string"; parameters = None }
    let Bool = { name = "bool"; parameters = None }

    let TypeMap = Map.ofList [ "int", Int;
                                "float", Float;
                                "string", String;
                                "bool", Bool ]