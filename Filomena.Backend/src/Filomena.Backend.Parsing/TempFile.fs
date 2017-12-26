namespace Filomena.Backend.Parsing

open System.IO

type TempFile (fileName, content) = 
    do File.WriteAllText (fileName, content)

    interface System.IDisposable with
        member __.Dispose () = 
            File.Delete fileName

    member __.Name = fileName