namespace Filomena.Backend.ParsingTest

open Xunit
open Filomena.Backend.Parsing

module UntypedParserTest =
    let mockPlatform = Platform ()

    [<Fact>]
    let ``"Hello, world!" parsing test failed because of no module name`` () =
        let source = "do printfn \"Hello, world!\""
        try
            do UntypedParser.parseAndCheckScript mockPlatform source |> ignore
            Assert.False true
        with
        | UntypedParser.CheckingException (msg, range) -> Assert.True ("Module name must be defined" = msg)
        
    [<Fact>]
    let ``Successfull "Hello, world!" parsing test with module definition`` () = 
        let source = """
            module A
            do printfn "Hello, world!"
        """
        do UntypedParser.parseAndCheckScript mockPlatform source |> ignore
        
        