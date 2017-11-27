namespace Filomena.Backend.Parsing

open Filomena.Backend.Models
open Exceptions

type Operation = { name: string;
                   inputs: string list;
                   output: string;
                   dependencies: Operation Set }

type MnemonicOrigin = Const of (string * DataType) | Output of Operation | Alias of string

type ProgramDiff = { addedMnemonics: (string, MnemonicOrigin) Map;
                     addedNames: string Set;
                     removedMnemonics: (string, MnemonicOrigin) Map;
                     removedNames: string Set }

module ProgramDiff = 
    let empty = { addedMnemonics = Map.empty
                  addedNames = Set.empty
                  removedMnemonics = Map.empty
                  removedNames = Set.empty }

type ParsedProgram = { mnemonics: (string, MnemonicOrigin) Map;
                       usedNames: string Set } with
    // TODO: test!
    static member (-) (next, prev) = 
        let calcMapAdds mapNext mapPrev = 
            mapNext
            |> Map.filter (fun key value ->
                match mapPrev |> Map.tryFind key with
                | Some origin -> origin <> value
                | None -> true)
        let calcSetAdds setNext setPrev = 
            setNext
            |> Set.filter (fun value -> not (setPrev |> Set.contains value))
        { addedMnemonics = calcMapAdds next.mnemonics prev.mnemonics;
          addedNames = calcSetAdds next.usedNames prev.usedNames;
          removedMnemonics = calcMapAdds prev.mnemonics next.mnemonics; 
          removedNames = calcSetAdds prev.usedNames next.usedNames }
    
    // TODO: test!
    static member (+) (program, diff) = 
        do expect (Map.isEmpty diff.removedMnemonics) "Removed mnemonics list must be empty"
        do expect (Set.isEmpty diff.removedNames) "Removed names list must be empty"
        let mnemonics = 
            diff.addedMnemonics
            |> Map.fold (fun accum key value -> Map.add key value accum) program.mnemonics
        let names = Set.union program.usedNames diff.addedNames in
        { mnemonics = mnemonics;
          usedNames = names }
        


module ParsedProgram = 
    let addMnemonic name origin parsedProgram = 
        { mnemonics = parsedProgram.mnemonics |> Map.add name origin;
          usedNames = parsedProgram.usedNames |> Set.add name }

    let rec escapeName parsedProgram name = 
        if parsedProgram.usedNames |> Set.contains name then
            escapeName parsedProgram (name + "'")
        else
            name

    let empty = { mnemonics = Map.empty;
                  usedNames = Set.empty }