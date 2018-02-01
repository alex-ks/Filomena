open Suave
open Suave.Filters
open Suave.Operators
open Suave.Filters
open Suave.Writers
open Suave.Successful

open Filomena.Backend.Parsing
open Newtonsoft.Json
open Filomena.Backend.Parsing.UntypedParser
open Filomena.Backend.SourceCodeServices
open Filomena.Backend.Models

open Filomena.Backend.WebApi

module Program = 
    type CompileRequest = { source: string }    

    let getBody request = request.rawForm |> System.Text.Encoding.UTF8.GetString

    let setCorsHeaders =
        setHeader  "Access-Control-Allow-Origin" "*"
        >=> setHeader "Access-Control-Allow-Headers" "content-type"

    let allowCors : WebPart =
        choose [
            OPTIONS >=>
                fun context ->
                    context |> (
                        setCorsHeaders
                        >=> OK "CORS approved" )
        ]

    let app = 
        POST >=> path "/" >=> request (fun r ->
            do printfn "%s" (getBody r)
            let { source = code } = JsonConvert.DeserializeObject<CompileRequest>(getBody r)
            use sourceServices = new SourceProvider ()
            match UntypedParser.parseAndCheckScript code with
            | Ok modulesList ->
                try
                    let optSources = 
                        modulesList
                        |> List.map (FsSourceProvider.getModuleSources sourceServices)
                    let program, errors = TypedParser.parse optSources code
                    if not (Seq.exists (fun e -> e.Severity = Error) errors) then
                        OK (JsonConvert.SerializeObject(program, Formatting.Indented, OptionConverter ()))
                    else
                        RequestErrors.BAD_REQUEST (string errors)
                with 
                | e -> RequestErrors.BAD_REQUEST (string e)
            | Failed (CheckErrors errors) -> 
                RequestErrors.BAD_REQUEST (string errors)
            | Failed (FSharpErrors errors) ->
                RequestErrors.BAD_REQUEST (string errors)) 
        >=> setHeader  "Access-Control-Allow-Origin" "*"
        >=> setHeader "Access-Control-Allow-Headers" "content-type"

    let config = defaultConfig
    

    [<EntryPoint>]
    let main _ =
        do startWebServer defaultConfig app
        0    
