namespace Filomena.Backend.Endpoint

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Giraffe
open App
open Microsoft.AspNetCore.Cors.Infrastructure
open System.Linq
open Newtonsoft.Json
open Giraffe.Serialization
open Filomena.Backend.ResolverClient
open Filomena.Backend.Parsing


type Startup() =
    let allowCors (builder: CorsPolicyBuilder) =
        builder.AllowAnyHeader()
            .AllowAnyMethod()
            .AllowAnyOrigin()
        |> ignore

    member __.ConfigureServices(services: IServiceCollection) =
        do ignore <| services.AddCors ()

        let optionSerializerSettings = 
            JsonSerializerSettings(
                Converters = [ OptionConverter() :> JsonConverter ].ToList(),
                Formatting = Formatting.Indented)

        do ignore <| services.AddGiraffe ()

        let resolverUrl = "http://ecclesia.ict.nsc.ru:27945"

        do ignore <| services.AddTransient<IResolver> (fun _ -> ResolverRestClient resolverUrl :> IResolver)
        do ignore <| services.AddTransient<Compiler> ()

        do ignore <| services.AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer(optionSerializerSettings))

    member __.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        if env.IsDevelopment() then 
            do ignore <| app.UseDeveloperExceptionPage ()
    
        do ignore <| app.UseCors allowCors

        do configureApp app
        