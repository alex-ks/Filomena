namespace Filomena.Backend.ResolverClient

open System.Net.Http
open Newtonsoft.Json
open System.Text
open FSharp.Control.Tasks
open System.Net
open System
open Newtonsoft.Json.Linq


type ResolverRestClient(resolverUrl) = 
    member __.ResolveAsync (atomIds: AtomId seq) = 
        use client = new HttpClient ()
        let queryContent = JsonConvert.SerializeObject atomIds
        let query = sprintf "%s/api/atoms?atoms=%s" resolverUrl queryContent
        in
        task  { let! response = client.GetAsync query
                let! body = response.Content.ReadAsStringAsync ()
                match response.StatusCode with
                | HttpStatusCode.OK -> 
                    return JsonConvert.DeserializeObject<AtomContent seq> body
                | _ ->
                    return raise (InvalidOperationException body) }

    member __.GetContentAsync (atomId: AtomId) =
        use client = new HttpClient ()
        let query = 
            let url = sprintf "%s/api/atoms/%s/%s" resolverUrl atomId.Kind atomId.Name
            match atomId.Version with
            | null -> url
            | version -> url + "?version=" + version
        in
        task  { let! response = client.GetAsync query
                let! body = response.Content.ReadAsStringAsync ()
                match response.StatusCode with
                | HttpStatusCode.OK ->
                    let contentResponse = JsonConvert.DeserializeObject<AtomContentResponse> body
                    return { Kind = contentResponse.Kind
                             Name = contentResponse.Name
                             Version = contentResponse.Version
                             AtomContent.Content = Convert.FromBase64String contentResponse.Content }
                | _ ->
                    return raise (InvalidOperationException body) }

    member __.CreateAsync (deps: AtomId seq) (content: AtomContent) =
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
        in
        task  { let! response = client.PostAsync (resolverUrl + "/atoms", content)
                let! body = response.Content.ReadAsStringAsync ()
                match response.StatusCode with
                | HttpStatusCode.OK -> 
                    return JsonConvert.DeserializeObject<AtomId> body
                | _ ->
                    return raise (InvalidOperationException body) }

    member __.GetInfoAsync (atomId: AtomId) =
        use client = new HttpClient ()
        let query = 
            let url = sprintf "%s/api/atoms/%s/%s/info" resolverUrl atomId.Kind atomId.Name
            match atomId.Version with
            | null -> url
            | version -> url + "?version=" + version
        in
        task  { let! response = client.GetAsync query
                let! body = response.Content.ReadAsStringAsync ()
                match response.StatusCode with
                | HttpStatusCode.OK ->
                    return JsonConvert.DeserializeObject<AtomInfo> body
                | _ ->
                    return raise (InvalidOperationException body) }

    interface IResolver with
        member this.ResolveAsync atomIds = this.ResolveAsync atomIds
        member this.GetContentAsync atomId = this.GetContentAsync atomId
        member this.CreateAsync depsIds content = this.CreateAsync depsIds content
        member this.GetInfoAsync atomId = this.GetInfoAsync atomId
        
and private AtomCreationRequest = { Kind: string
                                    Name: string
                                    Version: string
                                    Content: string
                                    Meta: JObject
                                    Dependencies: AtomId seq }
                                    
and private AtomContentResponse = { Kind: string
                                    Name: string
                                    Version: string
                                    Content: string }


        