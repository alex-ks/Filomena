namespace Filomena.Backend.SourceCodeServices

open System.IO

module Program = 
    let help = """
Usage: 
    add <module name> <source file name> for adding module to database
    update <module name> <source file name> for update module in database
    remove <module name> for removing module from database
"""

    let addModule (moduleName, sourceFile) = 
        use provider = new SourceProvider ()
        let sources = File.ReadAllText sourceFile
        do provider.AddModule (moduleName, sources)        

    let removeModule moduleName = 
        use provider = new SourceProvider ()
        do provider.RemoveModule moduleName

    let updateModule (moduleName, sourceFile) = 
        use provider = new SourceProvider ()
        let sources = File.ReadAllText sourceFile
        do provider.UpdateModule (moduleName, sources)

    [<EntryPoint>]
    let main (args: string[]) = 
        try
            match args with 
            | [|"add"; moduleName; sourceFile|] ->
                addModule (moduleName, sourceFile)

            | [|"update"; moduleName; sourceFile|] ->
                updateModule (moduleName, sourceFile)

            | [|"remove"; moduleName|] ->
                removeModule moduleName

            | [|"--help"|] -> 
                do printfn "%s" help

            | _ -> 
                do printfn "Invalid arguments! %s" help
        with
        | e -> 
            do printfn "Error: %A" e
        0