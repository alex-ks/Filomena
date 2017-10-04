namespace Filomena.Backend.ParsingTest

open Xunit
open Filomena.Backend.Parsing
open Filomena.Backend.Parsing.UntypedParser

module UntypedParserTest =
    [<Fact>]
    let ``"Hello, world!" parsing test failed because of no module name`` () =
        let source = "do printfn \"Hello, world!\""
        
        let parseResult = parseAndCheckScript source in
        match parseResult with
        | Failed (CheckErrors [CheckError (msg, _)]) ->
            do Assert.Equal(expected = "Module name must be defined", actual = msg)
        | Failed (CheckErrors _) -> 
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
        // TODO: implement
        Assert.False true
        
    [<Fact>]
    let ``Script parsing failed because of multiple modules`` () = 
        // TODO: implement
        Assert.False true
        
    [<Fact>]
    let ``Script parsing failed because of namespace declaration`` () = 
        // TODO: implement
        Assert.False true
        
        