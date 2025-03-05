using System;
using System.Collections.Generic;
using Microsoft.EntityFrameworkCore;

namespace places_api.Models;

public partial class PlacesContext : DbContext
{
    public PlacesContext()
    {
    }

    public PlacesContext(DbContextOptions<PlacesContext> options)
        : base(options)
    {
    }

    public virtual DbSet<Place> Places { get; set; }

    protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
#warning To protect potentially sensitive information in your connection string, you should move it out of source code. You can avoid scaffolding the connection string by using the Name= syntax to read it from configuration - see https://go.microsoft.com/fwlink/?linkid=2131148. For more guidance on storing connection strings, see https://go.microsoft.com/fwlink/?LinkId=723263.
        => optionsBuilder.UseSqlServer("Data Source=.\\sqlexpress;Initial Catalog=Assessment8; Integrated Security=SSPI;Encrypt=false;TrustServerCertificate=True;");

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.Entity<Place>(entity =>
        {
            entity.HasKey(e => e.Id).HasName("PK__Place__3214EC27DBDF8F66");

            entity.ToTable("Place");

            entity.Property(e => e.Id).HasColumnName("ID");
            entity.Property(e => e.Name).HasMaxLength(255);

            entity.HasData(
                new Place() { Id = 1, Name = "New York City", FirstTime = true },
                new Place() { Id = 2, Name = "Detroit", FirstTime = false },
                new Place() { Id = 3, Name = "Oklahoma", FirstTime = true }
            );
        });

        OnModelCreatingPartial(modelBuilder);
    }

    partial void OnModelCreatingPartial(ModelBuilder modelBuilder);
}
