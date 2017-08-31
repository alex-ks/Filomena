module Program

open Filomena.Backend.ScriptParser.Parser

let source = """
open IrisDataset
open Ml.Knn

let rec fact x = if x <= 1 then 1 else x * fact (x - 1)

let dataset = loadIrisDataset ()
let testSize = Seq.length dataset / 3

let testSet = dataset |> Seq.take testSize
let trainSet = dataset |> Seq.skip testSize
let test = testSet |> Seq.map fst
let train = trainSet |> Seq.map fst
let testAns = testSet |> Seq.map snd
let trainAns = trainSet |> Seq.map snd

let irisModel = trainKnn 4 (train, trainAns)
let classifiedIris = classify irisModel test
let result = score testAns classifiedIris
"""

[<EntryPoint>]
let main argv = 
    parseScript source
    0 // return an integer exit code