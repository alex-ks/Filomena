namespace Filomena.Backend.Parsing

type ('t, 'e) Maybe = Ok of 't | Failed of 'e

module Maybe = 
    type MaybeBuilder () = 
        member this.Return x = Ok x

        member this.Bind (t, f) = 
            match t with
            | Ok x -> f x
            | Failed e -> Failed e

    let maybe = MaybeBuilder ()

