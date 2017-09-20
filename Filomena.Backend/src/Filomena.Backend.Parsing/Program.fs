module Program

open Filomena.Backend.Parsing

[<EntryPoint>]
let main args =
    let source = """
    module A
    do ignore ()
    """
    printfn "%s" (UntypedParser.parseAndCheckScript (Platform ()) source)
    0