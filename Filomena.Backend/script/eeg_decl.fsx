module Eeg
    type Eeg = (string, float list) Map

    let loadEeg (id: string) : Eeg = 
        do ignore id
        Map.empty

    let dropChannel name eeg = 
        Map.remove name eeg

    let dropChannels names eeg = 
        let namesSet = Set.ofList names
        eeg
        |> Map.filter (fun key _ -> not (Set.contains key namesSet))

    let filterFrequences (low: int) (high: int) eeg = 
        do ignore low
        do ignore high
        eeg
