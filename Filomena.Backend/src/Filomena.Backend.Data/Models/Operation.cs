using System;
using System.Collections.Generic;
using System.Text;

namespace Filomena.Backend.Data.Models
{
    public class Operation
    {
        public string Name { get; set; }
        public IList<string> Input { get; set; }
        public IList<string> Output { get; set; }
    }
}
