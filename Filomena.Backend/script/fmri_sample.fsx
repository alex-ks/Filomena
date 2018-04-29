#load "fmri_decl.fsx"
#load "plot_decl.fsx"

open Fmri
open Plot

let f = loadFmri "path/to/fmri"
let rawFmri = rawData f

let time, data = List.unzip rawFmri

let cubes = data |> List.map (sliceBySize (5, 5, 5))

let cubes' = rearrange cubes

let calcCubeDistributions cube =
    let [compA; compB; compC] = (flatten >> histcounts >> fitNormal 3) cube
    in compA, compB, compC

let components = Map.map (fun _ cubeSeries -> List.map calcCubeDistributions cubeSeries) cubes'

for pair in components do
    let coordinate = pair.Key
    let compA, compB, compC = List.unzip3 pair.Value
    do plot time compA (sprintf "Component A of cube at %A" coordinate)
    do plot time compB (sprintf "Component B of cube at %A" coordinate)
    do plot time compC (sprintf "Component C of cube at %A" coordinate)