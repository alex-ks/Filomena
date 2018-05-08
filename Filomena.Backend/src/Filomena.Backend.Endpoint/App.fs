module Filomena.Backend.Endpoint.App

open Giraffe
open Giraffe.HttpStatusCodeHandlers
open Newtonsoft.Json
open Microsoft.AspNetCore.Builder

open Filomena.Backend.Parsing
open Filomena.Backend.Parsing.PartialParser
open Microsoft.AspNetCore.Http
open Filomena.Backend.ResolverClient

type CompileRequest = { source: string } 

type PartialResponse = { name: string; opens: string list }  

let compile { source = code }: HttpHandler = fun next context -> 
    let compiler = context.GetService<Compiler> ()
    task {
        try
            let! program = compiler.Compile code
            return! (json program next context)
        with 
        | CheckException errors -> return! RequestErrors.BAD_REQUEST (string errors) next context
        | e -> return! RequestErrors.BAD_REQUEST (string e) next context
    }


let partialCheck { source = code } =
    try
        let name, opens = partialParse code
        json { name = name; opens = opens }
    with
    | CheckException errors -> RequestErrors.BAD_REQUEST (string errors)
    | e -> RequestErrors.BAD_REQUEST (string e)

let webApp = 
    choose [ POST
             >=> choose [ route "/" >=> bindModel<CompileRequest> None compile
                          route "/partial" >=> bindModel<CompileRequest> None partialCheck ] ]
           

let configureApp (app : IApplicationBuilder) =
    // Add Giraffe to the ASP.NET Core pipeline
    app.UseGiraffe webApp