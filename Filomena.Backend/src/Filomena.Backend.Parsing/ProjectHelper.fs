namespace Filomena.Backend.Parsing

open System
open System.IO

module ProjectHelper = 
    let tempFileNamePrefix = "tmp"

    let tempFileName () = Path.ChangeExtension (Path.GetTempFileName (), "fsx")
        
    let projectFromScript source = 
        let name = tempFileName ()
        do File.WriteAllText (name, source)
        name
        
    let emptyProject () = projectFromScript String.Empty
    
    let isTemp (name: string) = 
        name.ToLower().StartsWith(tempFileNamePrefix)
