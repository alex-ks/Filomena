namespace Filomena.Backend.Parsing

open Filomena.Backend.ResolverClient
open Exceptions
open UntypedParser
open FSharp.Control.Tasks
open System.Threading.Tasks
open System.Text
open System

type Compiler(resolver: IResolver) = 
    let wait (t: 'a Task) = 
        t.GetAwaiter().GetResult()

    let getOriginAsync (wfs, decls) md = 
        task {
            let! isDecl = resolver.ExistsAsync ({ Kind = "fsdecl"; Name = md; Version = null })
            let! isWf = resolver.ExistsAsync ({ Kind = "filomena"; Name = md; Version = null }) in
            return
                match isDecl, isWf with
                | true, true -> md::wfs, md::decls
                | true, false -> md::wfs, decls
                | false, true -> wfs, md::decls
                | false, false -> 
                    md |> sprintf "Module %s does not exist" |> ArgumentException |> raise
        }

    let getOrigin lists md = getOriginAsync lists md |> wait

    let getOriginsOf modulesList = List.fold getOrigin ([], []) modulesList

    let getSourceAsync kind versionOpt name = 
        task {
            let! content = 
                match versionOpt with
                | Some version ->
                    resolver.GetContentAsync ({ Kind = kind; Name = name; Version = version })
                | None ->
                    resolver.GetContentAsync ({ Kind = kind; Name = name; Version = null })
            return Encoding.UTF8.GetString content.Content                
        }        

    let gatherSourcesOf modulesList =
        let wfs, decls = getOriginsOf modulesList
        let wfsContent = 
            wfs 
            |> List.map ((getSourceAsync "filomena" None) >> wait)
        let declsContent = 
            decls
            |> List.map ((getSourceAsync "fsdecl" None) >> wait)
        wfsContent, declsContent

    member __.Compile workflow = 
        match parseAndCheckScript workflow with
        | Ok modulesList ->
            let _workflows, declarations = gatherSourcesOf modulesList
            let program, errors = TypedParser.parse declarations workflow in
            if not (Seq.exists (fun e -> e.Severity = ErrorSeverity.Error) errors) then
                program
            else
                errors |> Seq.toList |> checkFailed
        | Error (UntypedCheckErrors errors) -> 
            errors |> List.map (ParsingError.ofUntypedCheckError) |> checkFailed
        | Error (FSharpErrors errors) ->
            ParsingError.ofFSharpErrorInfos errors |> Seq.toList |> checkFailed