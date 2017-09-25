module Program

open Filomena.Backend.Parsing

[<EntryPoint>]
let main args =
    let source = """
    module A
    do ignore ()
    """
    printfn "%A" (UntypedParser.parseAndCheckScript source)
    0