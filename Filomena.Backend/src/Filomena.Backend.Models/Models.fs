namespace Filomena.Backend.Models

open System.Collections.Generic

type DataType = { name: string; parameters: DataType list option }

type Operation = { id: int; name: string; input: DataType list; output: DataType list }

type MnemonicValue = { dataType: DataType; value: string }

type ComputationGraph = { operations: Operation list; dependencies: int list list; mnemonicsTable: (string, MnemonicValue) IDictionary }
        
module ComputationGraph = 
    let empty () = { operations = []; dependencies = []; mnemonicsTable = dict [] }