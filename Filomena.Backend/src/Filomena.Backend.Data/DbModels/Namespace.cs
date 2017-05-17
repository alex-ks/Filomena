using System;
using System.Collections.Generic;
using System.Text;

namespace Filomena.Backend.Data.DbModels
{
    internal class Namespace : IKeyEquatable<Namespace>
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public bool IsPublic { get; set; }

        public bool KeyEquals(Namespace other)
        {
            return Id == other.Id;
        }
    }
}
