namespace Filomena.Backend.ParsingTest

open Xunit
open Filomena.Backend.Parsing
open UntypedParser

module UntypedParserTest =
    [<Fact>]
    let ``"Hello, world!" parsing test failed because of no module name`` () =
        let source = "do printfn \"Hello, world!\""
        
        let parseResult = parseAndCheckScript source in
        match parseResult with
        | Error (UntypedCheckErrors [UntypedCheckError (msg, _)]) ->
            do Assert.Equal(expected = "Module name must be defined", actual = msg)
        | Error (UntypedCheckErrors _) -> 
            do Assert.True (false, "Unexpected error")
        | _ -> 
            do Assert.True (false, "Unexpected function result")

        
    [<Fact>]
    let ``Successfull "Hello, world!" parsing test with module definition`` () = 
        let source = """
            module A
            do printfn "Hello, world!"
        """
        let parseResults = parseAndCheckScript source in
        match parseResults with
        | Ok [] -> 
            do Assert.True true
        | _ -> 
            do Assert.True (false, "Unexpected function result")
        
        
    [<Fact>]
    let ``Successfull "Hello, world" parsing test with two open's`` () =
        let source = """
            module A
            
            open B
            open C.D
            
            do printfn "Hello, world!"
        """
        let parseResults = parseAndCheckScript source in
        match parseResults with
        | Ok [moduleB; moduleCD] -> 
            do Assert.Equal (expected = "B", actual = moduleB)
            do Assert.Equal (expected = "C.D", actual = moduleCD)
        | Ok _ -> 
            do Assert.True (false, "Wrong open's list size")
        | _ -> 
            do Assert.True (false, "Function failed")
            
    [<Fact>]
    let ``Script parsing failed because of nested modules`` () =
        let source = """
            module A
            
            module B = 
                let str = "Hello, world!"
        """
        let parseResults = parseAndCheckScript source in
        match parseResults with
        | Error (UntypedCheckErrors [UntypedCheckError (msg, _)]) ->
            Assert.Equal (expected = ParsingResources.nestedModulesNotAllowedMsg, actual = msg)
        | _ ->
            Assert.True (false, "Submodule error must be returned")
        
    [<Fact>]
    let ``Script parsing failed because of namespace declaration`` () = 
        let source = """
            namespace A
            
            let str = "Hello, world!"
        """
        let parseResults = parseAndCheckScript source in
        match parseResults with
        | Error (UntypedCheckErrors [UntypedCheckError (msg, _)]) ->
            Assert.Equal (expected = ParsingResources.namespaceIsNotAllowed, actual = msg)
        | _ ->
            Assert.True (false, "Namespace error must be returned")
        
        