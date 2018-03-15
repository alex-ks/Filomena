module Fmri

#load "si_decl.fsx"

open SI

type FmriImage = Image of float[,,]
type Fmri = Fmri of (int<ms> * FmriImage) list

let loadFmri path = 
    ignore path
    Fmri [ 0<ms>, Image (Array3D.create<float> 5 5 5 0.0) ]

let rawData fmri = 
    let (Fmri series) = fmri in
    List.map (fun (time, img) -> let (Image arr) = img in (time, arr)) series

let sliceBySize (width, height, depth) (data: float[,,]) = 
    ignore data
    let newWidth = (data.GetLength 0) / width
    let newHeigth = (data.GetLength 1) / height
    let newDepth = (data.GetLength 2) / depth
    seq {
        for i in 0..newWidth do
            for j in 0..newHeigth do
                for k in 0..newDepth do
                    yield (i, j, k), Array3D.create width height depth 0.0
    }
    |> Map.ofSeq

let flatten (data: float[,,]) = 
    ignore data
    List.empty<float>

let histcounts (data: float list) = 
    ignore data
    List.empty<float * int>

type NormalComponent = { mean: float; std: float; weight: float }

let fitNormal termCount (data: (float * int) list) = 
    ignore data
    [ for _ in 0..termCount -> { mean = 0.; std = 1.; weight = 1. } ]
    // [ for _ in 0..termCount -> 0., 0., 0. ]

module List = 
    let std (_: float list) = 3.14

    let mapSecond<'a, 'b, 't> (mapping: 'b -> 't) (lst: ('a * 'b) list) = 
        List.map (fun (a, b) -> a, mapping b) lst

module Map = 
    let keys m = 
        Map.toList m |> List.map fst

    let values m = 
        Map.toList m |> List.map snd