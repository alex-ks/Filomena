module Program

open Filomena.Backend.Parsing
open Microsoft.FSharp.Compiler.SourceCodeServices

let print x = printfn "%A" x

[<EntryPoint>]
let main args =
    let source = """
    module A
    let s:string = 7
    let loadInt (key: string) = int key
    let myAdd x y = x + y
    let myInc = myAdd 1
    let print = printfn "%A"
    let a = loadInt "2"
    let b = loadInt "3"
    let res = myAdd a b
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
        ()
    | [FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (mfv, mfvll, expr)] ->
        ()
    | [FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (mfv, mfvll, expr); _] ->
        ()
    | _ ->
        ()
    0