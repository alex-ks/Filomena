namespace Filomena.Backend.ParsingTest

open Xunit
open Filomena.Backend.Parsing
open PartialParser
open Filomena.Backend.ResolverClient
open System.Threading.Tasks
open Filomena.Backend.Models
open System.Text

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

    let [<Fact>] ``Script compiling, compiling 1 + 1 without opens, successfull parsing`` () = 
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

    let [<Fact>] ``Stript compiling, compiling two independent ops, empty dependencies got`` () =
        let source = """
            module A
            let a = 1 + 1
            let b = 2 * 2
        """
        let expectedDependencies = 
            Set.ofList<int> [] 
            |> List.replicate 2
        let compiler = ResolverMock [] |> Compiler

        let compiled = (compiler.Compile source).Result

        do Assert.Equal<int Set List> (expectedDependencies, compiled.dependencies)

    type TestDeps = (string * string, (string * string) Set) Map

    let extractDependencies program = 
        program.operations
        |> List.map (fun op ->
            let dependencies = 
                program.dependencies.[op.id]
                |> Set.map (fun id -> 
                    let dep = (List.find (fun op -> op.id = id) program.operations)
                    dep.name, List.head dep.output)
            (op.name, List.head op.output), dependencies)
        |> Map.ofList

    let [<Fact>] ``Stript compiling, compiling three mixed dependency ops, correct dependencies got`` () =
        let source = """
            module A
            let a = 1 + 1
            let b = 2 * a
            let c = 5 - 3
        """
        let expectedDependencies = 
            [ ("+", "A.a"), Set.ofList []
              ("*", "A.b"), Set.ofList [ ("+", "A.a") ]
              ("-", "A.c"), Set.ofList [] ]
            |> Map.ofList
        
        let compiler = ResolverMock [] |> Compiler

        let compiled = (compiler.Compile source).Result

        do Assert.Equal<TestDeps> (expectedDependencies, extractDependencies compiled)

    let [<Fact>] ``Script compiling, importing declaration, successful import`` () = 
        let sourceForImport = """
            module B
            let myAdd x y = x + y
        """
        let source = """
            module A
            open B
            let a = myAdd 1 1
        """
        let importAtom = { Kind = "fsdecl"
                           Name = "B"
                           Version = null
                           Content = Encoding.UTF8.GetBytes sourceForImport }
        
        let compiler = ResolverMock [ importAtom ] |> Compiler
        let compiled = (compiler.Compile source).Result

        match compiled.operations with
        | [ myAddOp ] -> 
            do Assert.Equal ("B.myAdd", myAddOp.name)
        | ops -> 
            do Assert.False (true, sprintf "unexpected operations: %A" ops)

    let [<Fact>] ``Script compiling, importing workflow, successful import`` () = 
        let sourceForImport = """
            module B
            let b = 1 + 1
        """
        let source = """
            module A
            open B
            let a = 2 + b
            let c = 3 * 3
        """
        let importAtom = { Kind = "filomena"
                           Name = "B"
                           Version = null
                           Content = Encoding.UTF8.GetBytes sourceForImport }
        let expectedDependencies = 
            [ ("+", "B.b"), Set.ofList []
              ("+", "A.a"), Set.ofList [ ("+", "B.b") ]
              ("*", "A.c"), Set.ofList [] ]       
            |> Map.ofList    
        
        let compiler = ResolverMock [ importAtom ] |> Compiler
        let compiled = (compiler.Compile source).Result

        do Assert.True (List.length compiled.operations = 3)
        do Assert.Equal<TestDeps> (expectedDependencies, extractDependencies compiled)

    let [<Fact>] ``Script compiling, importing workflow with sequence point, cross-workflow deps found`` () = 
        let sourceForImport = """
            module B
            do ignore ()
        """
        let source = """
            module A
            open B
            let a = 2 + 2
        """
        let importAtom = { Kind = "filomena"
                           Name = "B"
                           Version = null
                           Content = Encoding.UTF8.GetBytes sourceForImport }
        let ignoreOp = "Microsoft.FSharp.Core.Operators.ignore", "@Microsoft.FSharp.Core.Operators.ignoreOutput"
        let expectedDependencies = 
            [ ignoreOp, Set.ofList []
              ("+", "A.a"), Set.ofList [ ignoreOp ] ]
            |> Map.ofList    
        
        let compiler = ResolverMock [ importAtom ] |> Compiler
        let compiled = (compiler.Compile source).Result

        do printfn "%A" compiled

        do Assert.True (List.length compiled.operations = 2)
        do Assert.Equal<TestDeps> (expectedDependencies, extractDependencies compiled)
        