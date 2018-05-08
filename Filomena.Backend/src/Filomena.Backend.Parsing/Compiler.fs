namespace Filomena.Backend.Parsing

open Filomena.Backend.ResolverClient
open Exceptions
open UntypedParser
open FSharp.Control.Tasks
open System.Threading.Tasks
open System.Text
open System

type ModuleDesc = { name: string; version: string }

type Module = Workflow of ModuleDesc | Declaration of ModuleDesc

module Module = 
    let [<Literal>] WorkflowKind = "filomena"
    let [<Literal>] DeclKind = "fsdecl"

    let toAtom md = 
        match md with
        | Workflow desc -> { Kind = WorkflowKind; Name = desc.name; Version = desc.version }
        | Declaration desc -> { Kind = DeclKind; Name = desc.name; Version = desc.version }

    let ofAtom (atom: AtomId) = 
        let desc = { name = atom.Name; version = atom.Version } in
        match atom.Kind with
        | WorkflowKind -> Workflow desc
        | DeclKind -> Declaration desc
        | any -> any |> sprintf "Kind %s is not supported" |> ArgumentException |> raise

type Compiler(resolver: IResolver) = 
    let getOriginAsync modulesTask md = 
        task {
            let! modules = modulesTask
            let declSuspect = Declaration { name = md; version = null }
            let! isDecl = resolver.ExistsAsync (declSuspect |> Module.toAtom)
            let wfSuspect = Workflow { name = md; version = null }
            let! isWf = resolver.ExistsAsync (wfSuspect |> Module.toAtom) in
            return
                match isDecl, isWf with
                | true, true -> wfSuspect :: declSuspect :: modules
                | true, false -> declSuspect :: modules
                | false, true -> wfSuspect :: modules
                | false, false -> 
                    md |> sprintf "Module %s does not exist" |> ArgumentException |> raise
        }

    let getOriginsAsync modulesList = 
        task {
            let! modules = List.fold getOriginAsync (Task.FromResult []) modulesList
            return List.rev modules
        }

    let gatherSourcesAsync modules =
        task {
            let! contents = 
                modules
                |> Seq.map Module.toAtom
                |> resolver.ResolveAsync
            let mdSources = 
                contents
                |> Seq.map (fun content -> Encoding.UTF8.GetString content.Content)
                |> Seq.zip modules
            return 
                mdSources
                |> Seq.map (fun (md, source) -> 
                    match md with
                    | Workflow _ -> WorkflowSource source
                    | Declaration _ -> DeclSource source)
                |> Seq.toList                
        }
        

    member __.Compile workflow = 
        task {
            match parseAndCheckScript workflow with
            | Ok modulesList ->
                let! modules = getOriginsAsync modulesList
                let! sourcesForOpen = (gatherSourcesAsync modules)
                let sources = Seq.append sourcesForOpen [WorkflowSource workflow]
                let program, errors = TypedParser.parse' sources in
                if not (Seq.exists (fun e -> e.Severity = ErrorSeverity.Error) errors) then
                    return program
                else
                    return errors |> Seq.toList |> checkFailed
            | Error (UntypedCheckErrors errors) -> 
                return errors |> List.map (ParsingError.ofUntypedCheckError) |> checkFailed
            | Error (FSharpErrors errors) ->
                return ParsingError.ofFSharpErrorInfos errors |> Seq.toList |> checkFailed
        }