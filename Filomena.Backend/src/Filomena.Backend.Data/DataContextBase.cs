using Microsoft.EntityFrameworkCore;
using System;
using System.Collections.Generic;
using System.Text;

namespace Filomena.Backend.Data
{
    /// <summary>
    /// Base class
    /// </summary>
    internal class DataContextBase : DbContext
    {
        protected DataContextBase(DbContextOptions options) : base(options)
        {

        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            //modelBuilder.HasDefaultSchema("public");
            //modelBuilder.Entity<DbMeasurement>().HasKey(x => new { x.UserId, x.TypeId, x.DeviceId, x.Start });
            //modelBuilder.Entity<DbMeasurement>().HasOne(p => p.DbType).WithMany().HasForeignKey(p => p.TypeId).IsRequired();
            //modelBuilder.Entity<DbMeasurement>().HasOne(p => p.DbDevice).WithMany().HasForeignKey(p => p.DeviceId).IsRequired();
            base.OnModelCreating(modelBuilder);
        }

        protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        {
            throw new NotImplementedException($"Method {nameof(DataContextBase)}.{nameof(OnConfiguring)} must be overridden in derivative");
            //optionsBuilder.UseNpgsql(@"User ID=cardiobackend;Password=statium123;Host=84.237.52.124;Port=5432;Database=cardio_test;Pooling=true;");
            //optionsBuilder.UseInMemoryDatabase();
        }
    }
}
