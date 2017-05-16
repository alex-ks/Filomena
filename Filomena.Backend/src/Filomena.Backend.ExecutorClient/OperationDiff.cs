using System;
using System.Collections.Generic;
using System.Runtime.Serialization;
using System.Text;

namespace Filomena.Backend.ExecutorClient
{
    [DataContract]
    public class OperationDiff
    {
        [DataMember(Name = "id")]
        public int Id { get; set; }

        [DataMember(Name = "status")]
        public OperationStatus Status { get; set; }
    }
}
