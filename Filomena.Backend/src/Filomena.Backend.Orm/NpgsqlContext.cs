using Microsoft.EntityFrameworkCore;

namespace Filomena.Backend.Orm
{
    public class NpgsqlContext : DbContext
    {
        // TODO: move to config
        private const string ConnectionString = 
            @"Server=217.79.61.87;Port=5432;Database=filomena_db;Username=filomena;Password=F#isC00l;";

        public DbSet<ModuleSource> ModuleSources { get; set; }

        protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        {
            base.OnConfiguring(optionsBuilder);
            optionsBuilder.UseNpgsql(ConnectionString);
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);

            modelBuilder
                .Entity<ModuleSource>()
                .HasKey(s => s.Module);

            modelBuilder
                .Entity<ModuleSource>()
                .Property(s => s.Source)
                .IsRequired();
        }
    }
}