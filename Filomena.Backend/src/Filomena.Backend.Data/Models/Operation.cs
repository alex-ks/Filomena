using System;
using System.Collections.Generic;
using System.Runtime.Serialization;
using System.Text;

namespace Filomena.Backend.Data.Models
{
    [DataContract]
    public class Operation
    {
        [DataMember(Name = "id")]
        public int Id { get; set; }
        [DataMember(Name = "name")]
        public string Name { get; set; }
        [DataMember(Name = "input")]
        public IList<string> Input { get; set; }
        [DataMember(Name = "output")]
        public IList<string> Output { get; set; }
    }
}
