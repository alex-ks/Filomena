namespace Filomena.Backend.SourceCodeServices

open Filomena.Backend.Orm
open Filomena.Backend.Models
open System

type SourceProvider () =
    let context = new NpgsqlContext ()

    member __.AddModule (moduleName, source) =
        do context.ModuleSources.Add (ModuleSource (Module = moduleName, Source = source)) |> ignore
        do context.SaveChanges () |> ignore

    member __.RemoveModule moduleName = 
        let pair = ModuleSource (Module = moduleName)
        do context.Attach pair |> ignore
        do context.Remove pair |> ignore
        do context.SaveChanges |> ignore

    member __.UpdateModule (moduleName, source) = 
        let pair = ModuleSource (Module = moduleName, Source = source)
        do context.Update pair |> ignore
        do context.SaveChanges |> ignore

    interface IDisposable with member __.Dispose () = context.Dispose ()

    interface IFsSourceProvider with
        member __.GetModuleSources name = 
            let moduleSource = query { for source in context.ModuleSources do
                                       where (source.Module = name)
                                       select source
                                       exactlyOne }
            moduleSource.Source

