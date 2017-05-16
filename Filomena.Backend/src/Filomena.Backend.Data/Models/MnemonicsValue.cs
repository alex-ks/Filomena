using System.Runtime.Serialization;

namespace Filomena.Backend.Data.Models
{
    [DataContract]
    public class MnemonicsValue
    {
        [DataMember(Name = "value")]
        public string Value { get; set; }
        [DataMember(Name = "type")]
        public DataType Type { get; set; }
    }
}
