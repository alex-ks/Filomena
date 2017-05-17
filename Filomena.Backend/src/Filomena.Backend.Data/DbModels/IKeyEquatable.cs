using System;
using System.Collections.Generic;
using System.Text;

namespace Filomena.Backend.Data.DbModels
{
    internal interface IKeyEquatable<T>
    {
        bool KeyEquals(T other);
    }
}
