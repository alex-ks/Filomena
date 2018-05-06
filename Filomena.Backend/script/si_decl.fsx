module SI

[<Measure>] type s
[<Measure>] type ms
[<Measure>] type m
[<Measure>] type mm
[<Measure>] type Hz = 1/s

module ms =
    let toSec (x: float<ms>) = x / 1000.<ms/s>
    let toSecInt (x: int<ms>) = x / 1000<ms/s>

module s = 
    let toMs (x: float<s>) = x * 1000.<ms/s>
    let toMsInt (x: int<s>) = x * 1000<ms/s>

