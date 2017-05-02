using System;
using System.Collections.Generic;
using System.Text;

namespace Filomena.Backend.Data.Models
{
    public class ComputationGraph
    {
        public IList<Operation> Operations { get; set; }
        public IList<IList<int>> Dependencies { get; set; }
        public IDictionary<string, MnemonicsValue> MnemonicsTable { get; set; }
    }
}
