module Program

open Filomena.Backend.Parsing
open Microsoft.FSharp.Compiler.SourceCodeServices

let print x = printfn "%A" x

[<EntryPoint>]
let main args =
    let source = """
    module A
    let x, y = 2, 3
    let z, t = 5, 6
    let seqP = 2; "atata"
    let s:string = 7
    let apply f x = f x
    let loadInt (key: string) = int key
    let myAdd x y = x + y
    let myInc = myAdd 1
    let print = printfn "%A"
    let doSmt (x, y) z =
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
    """
    //printfn "%A" (UntypedParser.parseAndCheckScript source)
    let (Ok checkResult) = TypedParser.checkSingleFileNoSettings source
    let partialAssemblySign = checkResult.PartialAssemblySignature
    let [moduleA] = partialAssemblySign.Entities |> Seq.toList
    //let [myAddFunc] = moduleA.MembersFunctionsAndValues |> Seq.toList
    //print myAddFunc.FullType
    
    let (Ok projCheckResult) = TypedParser.getTypedTree source 
    
    let [scriptFile] = projCheckResult.AssemblyContents.ImplementationFiles
    match scriptFile.Declarations with
    | [FSharpImplementationFileDeclaration.Entity (entity, declList)] ->
        do print (entity.MembersFunctionsAndValues |> Seq.toList)
        do print (declList |> Seq.toList)
        declList
        |> List.map (fun x ->
            match x with 
            | MemberOrFunctionOrValue (valOrf, valOfss, expr) ->
                do printfn "%A: %A of %A" valOrf valOfss valOrf.FullType
            | _ -> 
                do ignore ())
        |> ignore
        ()
    | [FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (mfv, mfvll, expr)] ->
        ()
    | [FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (mfv, mfvll, expr); _] ->
        ()
    | _ ->
        ()
    0