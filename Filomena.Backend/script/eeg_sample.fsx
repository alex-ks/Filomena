module MyWorkflow

#load "eeg_decl.fsx"

open Eeg

let raw = loadEeg "R013"

// let raw' = dropChannels ["NOSE"; "Empty1"; "Empty2"; "HEOG"] raw
let raw' = dropChannel "NOSE" raw

let filtered = filterFrequences 1 40 raw'
