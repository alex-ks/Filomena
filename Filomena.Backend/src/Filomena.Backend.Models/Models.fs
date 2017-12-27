namespace Filomena.Backend.Models

type DataType = { name: string; parameters: DataType list option }

type Operation = { id: int; 
                   name: string; 
                   input: string list; 
                   output: string list; 
                   parameters: DataType list option }

type MnemonicValue = { dataType: DataType; value: string }

type ComputationGraph = { operations: Operation list; 
                          dependencies: int Set list; 
                          mnemonicsTable: (string, MnemonicValue) Map }
        
module ComputationGraph = 
    let empty () = { operations = []; dependencies = []; mnemonicsTable = Map.empty }