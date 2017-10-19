namespace Filomena.Backend.Parsing

type ('t, 'e) Maybe = Ok of 't | Failed of 'e

module Maybe = 
    type MaybeBuilder () = 
        member this.Return x = Ok x

        member this.ReturnFrom (m: ('a, 'b) Maybe) = m

        member this.Bind (m, f) = 
            match m with
            | Ok x -> f x
            | Failed e -> Failed e

    let maybe = MaybeBuilder ()

