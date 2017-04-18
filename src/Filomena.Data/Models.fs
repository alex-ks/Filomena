namespace Filomena.Data

open System.Collections.Generic
open System.Runtime.Serialization

module Models = 
    [<AllowNullLiteral>]
    [<DataContract>]
    type DataType () =
        [<DataMember(Name = "name")>]
        member val Name: string = null with get, set
        [<DataMember(Name = "parameters")>]
        member val Parameters = List<string> () with get, set

    [<AllowNullLiteral>]
    [<DataContract>]
    type MnemonicsValue () = 
        [<DataMember(Name = "name")>]
        member val Name: string = null with get, set
        [<DataMember(Name = "type")>]
        member val Type: DataType = null with get, set

    [<AllowNullLiteral>]
    [<DataContract>]
    type Operation () = 
        [<DataMember(Name = "name")>]
        member val Name: string = null with get, set
        [<DataMember(Name = "input")>]
        member val Input = List<string> () with get, set
        [<DataMember(Name = "output")>]
        member val Output = List<string> () with get, set

    [<AllowNullLiteral>]
    [<DataContract>]
    type ComputationGraph () = 
        [<DataMember(Name = "operations")>]
        member val Operations = List<Operation> () with get, set
        [<DataMember(Name = "dependencies")>]
        member val Dependencies = List<List<int>> () with get, set
        [<DataMember(Name = "mnemonicsTable")>]
        member val MnemonicsTable = Dictionary<string, MnemonicsValue> () with get, set