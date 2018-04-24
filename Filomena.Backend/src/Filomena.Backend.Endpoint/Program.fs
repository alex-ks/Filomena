namespace Filomena.Backend.Endpoint

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging


module Program =
    let exitCode = 0

    let BuildWebHost args =
        let hostingConf = ConfigurationBuilder().AddJsonFile("hosting.json").Build() in
        WebHost
            .CreateDefaultBuilder(args)
            .UseStartup<Startup>()
            .UseUrls(hostingConf.["server.url"])
            .Build()

    [<EntryPoint>]
    let main args =
        BuildWebHost(args).Run()

        exitCode
