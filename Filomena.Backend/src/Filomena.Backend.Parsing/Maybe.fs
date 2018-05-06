namespace Filomena.Backend.Parsing

module Result = 
    type ResultBuilder () = 
        member __.Return x = Ok x

        member __.ReturnFrom (m: ('a, 'b) Result) = m

        member __.Bind (m, f) = 
            match m with
            | Result.Ok x -> f x
            | Result.Error e -> Result.Error e

    let result = ResultBuilder ()

