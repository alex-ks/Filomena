using System;
using System.Collections.Generic;
using System.Runtime.Serialization;
using System.Text;

namespace Filomena.Backend.Data.Models
{
    [DataContract]
    public class ComputationGraph
    {
        [DataMember(Name = "operations")]
        public IList<Operation> Operations { get; set; }
        [DataMember(Name = "dependencies")]
        public IList<IList<int>> Dependencies { get; set; }
        [DataMember(Name = "mnemonicsTable")]
        public IDictionary<string, MnemonicsValue> MnemonicsTable { get; set; }
    }
}
