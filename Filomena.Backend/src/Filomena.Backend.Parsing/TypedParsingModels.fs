namespace Filomena.Backend.Parsing

open Filomena.Backend.Models

type Operation = { name: string;
                   inputs: string list;
                   output: string;
                   dependencies: Operation list }

type MnemonicOrigin = Const of (string * DataType) | Operation of Operation | Alias of string

type ParsedProgram = { mnemonics: (string, MnemonicOrigin) Map;
                       usedNames: string Set }

module ParsedProgram = 
    let addMnemonic parsedProgram name origin = 
        { mnemonics = parsedProgram.mnemonics |> Map.add name origin ;
          usedNames = parsedProgram.usedNames |> Set.add name }

    let rec escapeName parsedProgram name = 
        if parsedProgram.usedNames |> Set.contains name then
            escapeName parsedProgram (name + "'")
        else
            name
