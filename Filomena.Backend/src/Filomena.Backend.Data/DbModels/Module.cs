using System;
using System.Collections.Generic;
using System.Text;

namespace Filomena.Backend.Data.DbModels
{
    internal class Module : IKeyEquatable<Module>
    {
        public int Id { get; set; }
        public int Name { get; set; }

        public int NamespaceId { get; set; }
        public Namespace Namespace { get; set; }

        public bool KeyEquals(Module other)
        {
            return Id == other.Id;
        }
    }
}
