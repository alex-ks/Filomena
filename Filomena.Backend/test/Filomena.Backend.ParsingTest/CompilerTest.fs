namespace Filomena.Backend.ParsingTest

open Xunit
open Filomena.Backend.Parsing
open PartialParser
open Filomena.Backend.ResolverClient
open System.Threading.Tasks
open Filomena.Backend.Models

module CompilerTest =
    type ResolverMock(atoms: AtomContent seq) = 
        interface IResolver with
            member __.ResolveAsync _ = Task.FromResult atoms
            member __.GetContentAsync id = 
                atoms
                |> Seq.find (fun atom -> atom.Kind = id.Kind && atom.Name = id.Name && atom.Version = id.Version)
                |> Task.FromResult
            member __.CreateAsync _ _ = failwith "Not implemented"
            member __.GetInfoAsync _ = failwith "Not implemented"
            member __.ExistsAsync id = 
                atoms
                |> Seq.exists (fun atom -> atom.Kind = id.Kind && atom.Name = id.Name && atom.Version = id.Version)
                |> Task.FromResult

    [<Fact>]
    let ``Script compiling, compiling 1 + 1 without opens, successfull parsing`` () = 
        let graph: ComputationGraph = {
            operations = [{ id = 0; 
                            name = "+"; 
                            input = ["@+Input0"; "@+Input1"]; 
                            output = ["A.a"]; 
                            parameters = Some [ { name = "int"; parameters = None }
                                                { name = "int"; parameters = None }
                                                { name = "int"; parameters = None } ] } ]
            dependencies = [ Set.ofList [] ]
            mnemonicsTable = 
                Map.ofList [
                    "@+Input0", { value = "1"; dataType = { name = "int"; parameters = None } }
                    "@+Input1", { value = "1"; dataType = { name = "int"; parameters = None } }
                ]
        }
        
        let source = """
            module A
            let a = 1 + 1
        """
        let compiler = ResolverMock [] |> Compiler

        let compiled = (compiler.Compile source).Result
        do Assert.Equal (graph, compiled)