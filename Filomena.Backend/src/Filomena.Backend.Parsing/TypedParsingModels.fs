namespace Filomena.Backend.Parsing

open Filomena.Backend.Models
open Exceptions

type Operation = { name: string
                   inputs: string list
                   output: string
                   parameters: DataType list option
                   dependencies: Operation Set }

type MnemonicOrigin = Const of (string * DataType) | Output of Operation | Alias of string

type ProgramDiff = { addedMnemonics: (string, MnemonicOrigin) Map
                     addedNames: string Set
                     removedMnemonics: (string, MnemonicOrigin) Map
                     removedNames: string Set }

module ProgramDiff = 
    let empty = { addedMnemonics = Map.empty
                  addedNames = Set.empty
                  removedMnemonics = Map.empty
                  removedNames = Set.empty }

type ParsedProgram = { mnemonics: (string, MnemonicOrigin) Map
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

    let toComputationGraph parsedProgram = 
        let operations = 
            parsedProgram.mnemonics
            |> Map.filter (fun _ value ->
                match value with 
                | Output _ -> true
                | _ -> false)
            |> Map.toList
            |> List.map (fun (_, value) ->
                match value with
                | Output operation -> operation
                | _ -> unexpected "Another origins must be filtered")
        let operationIndices = 
            operations
            |> List.mapi (fun i op -> op, i)
            |> Map.ofList
        let dependencies = 
            operations
            |> List.map (fun op ->
                let depsIndices = 
                    op.dependencies
                    |> Set.map (fun dep -> operationIndices.[dep]) in
                depsIndices, operationIndices.[op])
            |> List.sortBy (fun (_, i) -> i)
            |> List.map (fun (deps, _) -> deps)
        let mnemonics = 
            parsedProgram.mnemonics
            |> Map.filter (fun _ value ->
                match value with 
                | Const _ -> true
                | _ -> false)
            |> Map.map (fun _ value ->
                match value with
                | Const (value, t) -> { dataType = t; value = value }
                | _ -> unexpected "Another origins must be filtered")
        { operations = operations |> List.mapi (fun i op -> { Filomena.Backend.Models.Operation.id = i
                                                              name = op.name
                                                              input = op.inputs
                                                              output = [op.output]
                                                              parameters = op.parameters })
          dependencies = dependencies
          mnemonicsTable = mnemonics }            