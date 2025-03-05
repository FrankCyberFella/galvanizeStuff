﻿// <auto-generated />
using System;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.EntityFrameworkCore.Metadata;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using places_api.Models;

#nullable disable

namespace places_api.Migrations
{
    [DbContext(typeof(PlacesContext))]
    partial class PlaceContextModelSnapshot : ModelSnapshot
    {
        protected override void BuildModel(ModelBuilder modelBuilder)
        {
#pragma warning disable 612, 618
            modelBuilder
                .HasAnnotation("ProductVersion", "8.0.7")
                .HasAnnotation("Relational:MaxIdentifierLength", 128);

            SqlServerModelBuilderExtensions.UseIdentityColumns(modelBuilder);

            modelBuilder.Entity("places_api.Models.Place", b =>
                {
                    b.Property<int>("Id")
                        .ValueGeneratedOnAdd()
                        .HasColumnType("int")
                        .HasColumnName("ID");

                    SqlServerPropertyBuilderExtensions.UseIdentityColumn(b.Property<int>("Id"));

                    b.Property<bool?>("FirstTime")
                        .HasColumnType("bit");

                    b.Property<string>("Name")
                        .HasMaxLength(255)
                        .HasColumnType("nvarchar(255)");

                    b.HasKey("Id")
                        .HasName("PK__Place__3214EC27DBDF8F66");

                    b.ToTable("Place", (string)null);

                    b.HasData(
                        new
                        {
                            Id = 1,
                            FirstTime = true,
                            Name = "New York City"
                        },
                        new
                        {
                            Id = 2,
                            FirstTime = false,
                            Name = "Detroit"
                        },
                        new
                        {
                            Id = 3,
                            FirstTime = true,
                            Name = "Oklahoma"
                        });
                });
#pragma warning restore 612, 618
        }
    }
}
