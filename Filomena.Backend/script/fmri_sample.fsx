#load "fmri_decl.fsx"

open Fmri

let f = loadFmri "path/to/fmri"
let rawFmri = rawData f

let time, data = List.unzip rawFmri

let cubes = data |> List.map (sliceBySize (5, 5, 5))

let calcFrameDistributions frame = 
    Map.map (fun _ cube -> cube |> flatten |> histcounts |> fitNormal 3) frame

let distributions = List.map calcFrameDistributions cubes

let cubeKeys = Map.keys (List.head cubes)

let frames = List.zip time distributions

let a = 
    frames
    |> List.collect (fun (t, frame) ->
        frame
        |> Map.toList
        |> List.map (fun (index, components) -> index, t, components))
    |> List.groupBy (fun (index, _, _) -> index)
    
    

(*

есть список кадров-кубов
точнее есть список время-карта кубов
а хотим получить карту времени-кубов


хотим получить динамику по каждому индексу

*)

// let dynamics = 
//     cubeKeys 
//     |> List.map (fun key -> 
//         distributions
//         |> List.map (fun (t, distr) -> t, distr.[key]))
//     |> List.map (fun distrSeries -> 
//         distrSeries
//         |> List.map (fun (t, distr) ->
//             let [fstDistr; sndDistr; thrdDistr] = distr
//             t, (fstDistr, sndDistr, thrdDistr)))