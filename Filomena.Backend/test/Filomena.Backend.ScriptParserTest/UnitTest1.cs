using System;
using Xunit;
using Filomena.Backend.ScriptParser;

namespace Filomena.Backend.ScriptParserTest
{
    public class UnitTest1
    {
        [Fact]
        public void Test1()
        {
            var source = @"
open IrisDataset
open Ml.Knn

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
";
            Parser.parseScript(source);
        }
    }
}
