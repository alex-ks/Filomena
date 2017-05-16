using System;
using System.Collections.Generic;
using System.Runtime.Serialization;
using System.Text;

namespace Filomena.Backend.ExecutorClient
{
    [DataContract]
    public class ComputationGraphDiff
    {
        [DataMember(Name = "operations")]
        public IList<OperationDiff> Operations { get; set; }

        [DataMember(Name = "mnemonicsTable")]
        public IDictionary<string, string> MnemonicsTable { get; set; }
    }
}
