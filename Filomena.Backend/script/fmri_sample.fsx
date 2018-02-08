#load "fmri_decl.fsx"

open Fmri

let f = loadFmri "path/to/fmri"
let rawFmri = rawData f
let cubes = List.map (fun (t, raw) -> t, sliceBySize (5, 5, 5) raw) rawFmri

let calcFrameDistributions frame = 
    Map.map (fun _ cube -> cube |> flatten |> histcounts |> fitNormal 3) frame

let distributions = List.map (fun (t, frame) -> t, calcFrameDistributions frame) cubes

let cubeKeys = cubes |> List.head |> snd |> Map.keys

let dynamics = 
    cubeKeys 
    |> List.map (fun key -> 
        distributions
        |> List.map (fun (t, distr) -> t, distr.[key]))
    |> List.map (fun distrSeries -> 
        distrSeries
        |> List.map (fun (t, distr) ->
            let [fstDistr; sndDistr; thrdDistr] = distr
            t, (fstDistr, sndDistr, thrdDistr)))