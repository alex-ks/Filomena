module A
    let genName () = System.Guid.NewGuid ()
    let genName' = genName
    let x, y = 2, 3
    let z, t = 5, 6
    let seqP = 2; "atata"
    let s:string = "atata"
    let apply f x = f x
    let loadInt (key: string) = int key
    let myAdd x y = x + y
    let myInc = myAdd 1
    let someVal = (apply myInc) 42
    let anotherInc = fun x -> myInc x
    let someF = anotherInc
    let someF' = myInc
    let print x = printfn "%A" x
    let doSmt (x, y) z =
        let myAdd2 x y = x + y
        let v = myAdd2 2 2
        do print x
        let a = 1
        let a' = 1.5
        print y
        let b = 2
        do print z
        let c = 3
        do myInc |> apply |> ignore
        do (myAdd 2 2) |> myInc |> ignore
        let d = 4 in
        (x + y) * z
    let doSmt' pair z = 
        (fst pair + snd pair) * z
    let a = loadInt "2"
    let b = loadInt "3"
    let res = myAdd a b
    let pair = (2, 2)
    let _ = doSmt pair 2
    let _ = doSmt' pair 2
    let _ = doSmt (1, 2) 3
    let _ = doSmt' (1, 2) 3
    let g = 4
    let _ = doSmt (g, 5) 6
    do print res

