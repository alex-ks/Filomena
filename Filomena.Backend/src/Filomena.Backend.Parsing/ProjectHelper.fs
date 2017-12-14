namespace Filomena.Backend.Parsing

open System
open System.IO
open Exceptions

module ProjectHelper = 
    let tempFileNamePrefix = "tmp"
    let sdkVersion = "2.1.2"
    let fscorePath = sprintf "/usr/local/share/dotnet/sdk/%s/FSharp/FSharp.Core.dll" sdkVersion
    let fscorePath' = sprintf "/usr/share/dotnet/sdk/%s/FSharp/FSharp.Core.dll" sdkVersion
    
    let tempFileName () = Path.ChangeExtension (Path.GetTempFileName (), "fs")
        
    let projectFromScript source = 
        let name = tempFileName ()
        do File.WriteAllText (name, source)
        name
        
    let emptyProject () = projectFromScript String.Empty
    
    let isTemp (name: string) = 
        name.ToLower().StartsWith(tempFileNamePrefix)

    let sysLib name = 
        match Environment.OSVersion.Platform with
        | PlatformID.Unix ->
            let sysDir = Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory ()
            let (++) a b = System.IO.Path.Combine(a,b)    
            sysDir ++ name + ".dll"
        | _ ->
            (string Environment.OSVersion.Platform) |> notSupported

    let changeExtension ext fileName = 
        Path.ChangeExtension (fileName, ext)

    let changeToFsproj = changeExtension "fsproj"
    let changeToDll = changeExtension "dll"
    