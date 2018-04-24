namespace Filomena.Backend.ParsingTest

open Xunit
open Filomena.Backend.Parsing
open PartialParser

module PartialParserTest =
    [<Fact>]
    let ``Successfull "Hello, world!" parsing test with module definition`` () = 
        let source = """
            module A
            do printfn "Hello, world!"
        """
        let name, opens = partialParse source in
        do Assert.Empty opens
        do Assert.Equal("A", name)

    [<Fact>]
    let ``Successfull parsing test with module definition and script errors`` () = 
        let source = """
            module A
            do printfn 
        """
        let name, opens = partialParse source in
        do Assert.Empty opens
        do Assert.Equal("A", name)

    [<Fact>]
    let ``Successfull parsing test with module definition, opens and errors`` () = 
        let source = """
            module A
            open B
            do printfn 
        """
        let name, opens = partialParse source in
        let openName = Assert.Single opens
        do Assert.Equal("A", name)
        do Assert.Equal("B", openName)