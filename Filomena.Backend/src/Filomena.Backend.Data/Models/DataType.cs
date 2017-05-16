using System.Collections.Generic;
using System.Runtime.Serialization;

namespace Filomena.Backend.Data.Models
{
    [DataContract]
    public class DataType
    {
        [DataMember(Name = "name")]
        public string Name { get; set; }
        [DataMember(Name = "parameters")]
        public IList<DataType> Parameters { get; set; }
    }
}
