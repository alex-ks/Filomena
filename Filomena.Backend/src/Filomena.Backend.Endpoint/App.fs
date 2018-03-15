module Filomena.Backend.Endpoint.App

open Giraffe
open Giraffe.HttpStatusCodeHandlers
open Newtonsoft.Json
open Microsoft.AspNetCore.Builder

open Filomena.Backend.Parsing
open Filomena.Backend.Parsing.UntypedParser
open Filomena.Backend.SourceCodeServices
open Filomena.Backend.Models

type CompileRequest = { source: string }    

let compile { source = code } = 
    use sourceServices = new SourceProvider ()
    match UntypedParser.parseAndCheckScript code with
    | Ok modulesList ->
        try
            let optSources = 
                modulesList
                |> List.map (FsSourceProvider.getModuleSources sourceServices)
            let program, errors = TypedParser.parse optSources code
            if not (Seq.exists (fun e -> e.Severity = Error) errors) then
                // Successful.OK (JsonConvert.SerializeObject(program, Formatting.Indented, OptionConverter ()))
                json program
            else
                RequestErrors.BAD_REQUEST (string errors)
        with 
        | CheckException errors -> RequestErrors.BAD_REQUEST (string errors)
        | e -> RequestErrors.BAD_REQUEST (string e)
    | Failed (CheckErrors errors) -> 
        RequestErrors.BAD_REQUEST (string errors)
    | Failed (FSharpErrors errors) ->
        RequestErrors.BAD_REQUEST (string errors) 


let webApp = 
    choose [ POST 
             >=> route "/" 
             >=> bindModel<CompileRequest> None compile ]

let configureApp (app : IApplicationBuilder) =
    // Add Giraffe to the ASP.NET Core pipeline
    app.UseGiraffe webApp