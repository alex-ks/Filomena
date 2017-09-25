namespace Filomena.Backend.ParsingTest

open Xunit
open Filomena.Backend.Parsing
open Filomena.Backend.Parsing.UntypedParser

module UntypedParserTest =
    [<Fact>]
    let ``"Hello, world!" parsing test failed because of no module name`` () =
        let source = "do printfn \"Hello, world!\""
        
        let parseResult = parseAndCheckScript source in
        match parseAndCheckScript source with
        | Failed (CheckErrors errors) -> 
            match errors with
            | [CheckError (msg, _)] -> 
                do Assert.Equal(expected = "Module name must be defined", actual = msg)
            | _ -> 
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
        | Ok opensList ->
            do Assert.Equal (expected = 2, actual = List.length opensList)
            match opensList with
            | [moduleB; moduleCD] -> 
                do Assert.Equal (expected = "B", actual = moduleB)
                do Assert.Equal (expected = "C.D", actual = moduleCD)
            | _ -> 
                do Assert.True (false, "Wrong open's list size")
        | _ -> do Assert.True (false, "Function failed")
        