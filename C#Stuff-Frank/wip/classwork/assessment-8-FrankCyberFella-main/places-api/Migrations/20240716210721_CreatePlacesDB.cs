using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

#pragma warning disable CA1814 // Prefer jagged arrays over multidimensional

namespace places_api.Migrations
{
    /// <inheritdoc />
    public partial class CreatePlacesDB : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "Place",
                columns: table => new
                {
                    ID = table.Column<int>(type: "int", nullable: false)
                        .Annotation("SqlServer:Identity", "1, 1"),
                    Name = table.Column<string>(type: "nvarchar(255)", maxLength: 255, nullable: true),
                    FirstTime = table.Column<bool>(type: "bit", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK__Place__3214EC27DBDF8F66", x => x.ID);
                });

            migrationBuilder.InsertData(
                table: "Place",
                columns: new[] { "ID", "FirstTime", "Name" },
                values: new object[,]
                {
                    { 1, true, "New York City" },
                    { 2, false, "Detroit" },
                    { 3, true, "Oklahoma" }
                });
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "Place");
        }
    }
}
