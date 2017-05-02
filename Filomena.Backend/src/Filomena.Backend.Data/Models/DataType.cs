using System;
using System.Collections.Generic;
using System.Text;

namespace Filomena.Backend.Data.Models
{
    public class DataType
    {
        public string Name { get; set; }
        public IList<DataType> Parameters { get; set; }
    }
}
