namespace Filomena.Backend.Parsing

open System
open System.IO
open Exceptions
open System.Diagnostics
open System.Text.RegularExpressions

module String = 
    let trim (str: string) = str.Trim ()

module ProjectHelper = 
    let tempFileNamePrefix = "tmp"
    let sdkVersion = 
        let version = 
            if not (File.Exists "version.txt") then 
                use dotnet = new Process (StartInfo = ProcessStartInfo(FileName = "dotnet", 
                                                                       Arguments = "--version", 
                                                                       UseShellExecute = false, 
                                                                       RedirectStandardOutput = true, 
                                                                       CreateNoWindow = true))
                if dotnet.Start () && ((do dotnet.WaitForExit ()); dotnet.ExitCode = 0) then
                    let version = dotnet.StandardOutput.ReadToEnd () |> String.trim
                    do File.WriteAllText ("version.txt", version)
                    version
                else
                    failwith "Unable to get SDK version"
            else                              
                File.ReadAllText "version.txt" |> String.trim
        let validVersionFormat = Regex @"[0-9]+\.[0-9]+\.[0-9]+"
        if validVersionFormat.IsMatch version then
            version
        else
            failwith "Invalid SDK version format"
        

    let fscorePath = 
        match Environment.OSVersion.Platform, Environment.OSVersion.Version.Major with
        | PlatformID.Unix, 4 -> // Linux
            sprintf "/usr/share/dotnet/sdk/%s/FSharp/FSharp.Core.dll" sdkVersion
        | PlatformID.Unix, _ -> // MacOS
            sprintf "/usr/local/share/dotnet/sdk/%s/FSharp/FSharp.Core.dll" sdkVersion
        | _ ->
            unexpected "OS is currently unsupported"

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
    