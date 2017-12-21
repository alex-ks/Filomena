namespace Filomena.Backend.ParsingTest

open Xunit
open Filomena.Backend.Parsing

module TypedParserModelsTest =
    [<Fact>] 
    let ``Simple successfull program diff test`` () = 
        let mnemonics = 
            [ "a", Const ("2",  { name = "int"; parameters = None })
              "b", Const ("atata", { name = "string"; parameters = None}) ]
            |> Map.ofList
        
        let program = { mnemonics = mnemonics;
                        usedNames = Set.ofList ["a"; "b"] }
        let op = { name = "Calc"; inputs = ["a"; "b"]; output = "c"; parameters = None; dependencies = Set.empty }
        let program' = program |> ParsedProgram.addMnemonic op.output (Output op)
        
        let expectedDiff = { addedMnemonics = Map.ofList [op.output, Output op]
                             addedNames = Set.ofList [op.output]
                             removedMnemonics = Map.empty
                             removedNames = Set.empty }  
        let diff = program' - program
        
        do Assert.Equal (expected = expectedDiff, actual = diff)
        
    [<Fact>]
    let ``Successfull difference addition test`` () = 
        let mnemonics = 
                    [ "a", Const ("2",  { name = "int"; parameters = None })
                      "b", Const ("atata", { name = "string"; parameters = None}) ]
                    |> Map.ofList
                
        let program = { mnemonics = mnemonics;
                        usedNames = Set.ofList ["a"; "b"] }
                        
        let op1 = { name = "Calc"; inputs = ["a"; "b"]; output = "c"; parameters = None; dependencies = Set.empty }
        let op2 = { name = "Transform"; inputs = ["b"; "a"]; output = "d"; parameters = None; dependencies = Set.empty }
        
        let diff1 = (ParsedProgram.addMnemonic op1.output (Output op1) program) - program
        let diff2 = (ParsedProgram.addMnemonic op2.output (Output op2) program) - program
        
        let expected = 
            program
            |> ParsedProgram.addMnemonic op1.output (Output op1)
            |> ParsedProgram.addMnemonic op2.output (Output op2)
            
        do Assert.Equal (expected = expected, actual = program + diff1 + diff2)
        
        
            
                
        
        
        
        
