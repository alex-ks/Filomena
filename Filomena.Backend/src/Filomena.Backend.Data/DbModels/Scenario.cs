using Filomena.Backend.Models;
using System;
using System.Collections.Generic;
using System.Text;

namespace Filomena.Backend.Data.DbModels
{
    internal class Scenario : IKeyEquatable<Scenario>
    {
        public int Id { get; set; }
        public string Name { get; set; }

        public int NamespaceId { get; set; }
        public Namespace Namespace { get; set; }

        public ComputationGraph ComputationGraph { get; set; }

        public bool KeyEquals(Scenario other)
        {
            return Id == other.Id;
        }
    }
}
