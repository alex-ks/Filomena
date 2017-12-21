namespace Filomena.Backend.SourceCodeServices

open Filomena.Backend.Models
open FSharp.Data.Sql

module SourceProvider = 
    let [<Literal>] dbVendor = Common.DatabaseProviderTypes.POSTGRESQL
    let [<Literal>] connString = "Host=217.79.61.87;Database=filomena_db;Username=filomena;Password=F#isC00l"
    let [<Literal>] connexStringName = "DefaultConnectionString"
    let [<Literal>] resolutionPath = "./resolutionPath"
    
    type Npgsql = SqlDataProvider<Common.DatabaseProviderTypes.POSTGRESQL, connString, connexStringName, resolutionPath>

