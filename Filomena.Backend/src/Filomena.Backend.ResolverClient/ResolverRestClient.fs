namespace Filomena.Backend.ResolverClient

open System.Net.Http
open Newtonsoft.Json
open System.Text
open FSharp.Control.Tasks
open System.Net
open System
open Newtonsoft.Json.Linq

module NetModels = 
    type AtomContentResponse = { Kind: string
                                 Name: string
                                 Version: string
                                 Content: string }

    type AtomCreationRequest = { Kind: string
                                 Name: string
                                 Version: string
                                 Content: string
                                 Meta: JObject
                                 Dependencies: AtomId seq }

open NetModels

type ResolverRestClient(resolverUrl) = 
    let buildInfoUrl (atomId: AtomId) =
        let url = Uri (Uri resolverUrl, sprintf "/api/atoms/%s/%s/info" atomId.Kind atomId.Name)
        match atomId.Version with
        | null -> url
        | version -> Uri (url, "?version=" + version)

    member __.ResolveAsync (atomIds: AtomId seq) = 
        task {
            use client = new HttpClient ()
            let queryContent = JsonConvert.SerializeObject atomIds
            let uri = Uri (Uri resolverUrl, "/api/atoms?atoms=" + queryContent)
            let! response = client.GetAsync uri
            let! body = response.Content.ReadAsStringAsync () in
            match response.StatusCode with
            | HttpStatusCode.OK -> 
                return JsonConvert.DeserializeObject<AtomContent seq> body
            | _ ->
                return raise (InvalidOperationException body) 
        }

    member __.GetContentAsync (atomId: AtomId) = 
        task {
            use client = new HttpClient ()
            let query = 
                let url = Uri (Uri resolverUrl, sprintf "/api/atoms/%s/%s" atomId.Kind atomId.Name)
                match atomId.Version with
                | null -> url
                | version -> Uri (url, "?version=" + version)
            let! response = client.GetAsync query
            let! body = response.Content.ReadAsStringAsync () in
            match response.StatusCode with
            | HttpStatusCode.OK ->
                let contentResponse = JsonConvert.DeserializeObject<AtomContentResponse> body in
                return { Kind = contentResponse.Kind
                         Name = contentResponse.Name
                         Version = contentResponse.Version
                         AtomContent.Content = Convert.FromBase64String contentResponse.Content }
            | _ ->
                return raise (InvalidOperationException body) 
        }

    member __.CreateAsync (deps: AtomId seq) (content: AtomContent) = 
        task {
            use client = new HttpClient ()
            let request = 
                { Kind = content.Kind
                  Name = content.Name
                  Version = content.Version
                  Content = Convert.ToBase64String (content.Content)
                  Meta = null
                  Dependencies = deps }
            use content = 
                new StringContent (JsonConvert.SerializeObject request, Encoding.UTF8, "application/json")
            do printfn "%s" (JsonConvert.SerializeObject request)
            let! response = client.PostAsync (Uri (Uri resolverUrl, "/api/atoms"), content)
            let! body = response.Content.ReadAsStringAsync () in
            match response.StatusCode with
            | HttpStatusCode.OK -> 
                return JsonConvert.DeserializeObject<AtomId> body
            | _ ->
                return raise (InvalidOperationException body) 
        }

    member __.GetInfoAsync (atomId: AtomId) = 
        task {
            use client = new HttpClient ()
            let query = buildInfoUrl atomId
            let! response = client.GetAsync query
            let! body = response.Content.ReadAsStringAsync () in
            match response.StatusCode with
            | HttpStatusCode.OK ->
                return JsonConvert.DeserializeObject<AtomInfo> body
            | _ ->
                return raise (InvalidOperationException body) 
        }

    member __.ExistsAsync (atomId: AtomId) = 
        task {
            use client = new HttpClient ()
            let query = buildInfoUrl atomId
            in
            let! response = client.GetAsync query in
            return response.StatusCode = HttpStatusCode.OK
        }

    interface IResolver with
        member this.ResolveAsync atomIds = this.ResolveAsync atomIds
        member this.GetContentAsync atomId = this.GetContentAsync atomId
        member this.CreateAsync depsIds content = this.CreateAsync depsIds content
        member this.GetInfoAsync atomId = this.GetInfoAsync atomId
        member this.ExistsAsync atomId = this.ExistsAsync atomId
        
        