#r "packages/TaskBuilder.fs/lib/netstandard1.6/TaskBuilder.fs.dll"
#r "packages/Newtonsoft.Json/lib/netstandard2.0/Newtonsoft.Json.dll"
#r "../src/Filomena.Backend.ResolverClient/bin/Debug/netstandard2.0/Filomena.Backend.ResolverClient.dll"
#r "netstandard"

open System.Text
open System.Threading.Tasks
open System.IO

open Filomena.Backend.ResolverClient

let wait (t: Task) = t.Wait ()
let getResult (t: 'a Task) = t.Result

let client = (ResolverRestClient "http://localhost:7945") :> IResolver

let content = 
    { Kind = "text"; Name = "hello"; Version = null }
    |> client.GetContentAsync
    |> getResult

let text = Encoding.UTF8.GetString (content.Content)

do printfn "%s" text    

let siContent = File.ReadAllBytes "si_decl.fsx"

let siAtomContent = { Kind = "fsdecl"; Name = "SI"; Version = "0.0.1"; Content = siContent }

let siId = client.CreateAsync [] siAtomContent |> getResult

do printfn "%A" siId

let siAtom' =
    { Kind = "fsdecl"; Name = "SI"; Version = null }
    |> client.GetContentAsync 
    |> getResult

do printfn "%s" (Encoding.UTF8.GetString siAtom'.Content)

let eegContent = File.ReadAllBytes "eeg_decl.fsx"

let eegAtomContent = { Kind = "fsdecl"; Name = "Eeg"; Version = "0.0.1"; Content = eegContent }

let eegId = client.CreateAsync [siId] eegAtomContent |> getResult


do printfn "%A" eegId

for a in client.ResolveAsync [eegId] |> getResult do
    printfn "%A" a