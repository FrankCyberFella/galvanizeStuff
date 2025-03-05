using Microsoft.EntityFrameworkCore;
using moviesAPI.Models;

var builder = WebApplication.CreateBuilder(args);

// Add services to the container.

builder.Services.AddControllers();
// Learn more about configuring Swagger/OpenAPI at https://aka.ms/aspnetcore/swashbuckle
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();

builder.Services.AddDbContext<MoviedbContext>(option =>
    option.UseSqlServer(builder.Configuration.GetConnectionString("moviedb")));

var app = builder.Build();

// Add DbContext for server and database used in the app
// Server Connection String is stored in and obtained from appsettings.json file
//                   it contains the name of the server and database you are using
//                   and possibly some additional options



// Configure the HTTP request pipeline.
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseHttpsRedirection();

app.UseAuthorization();

app.MapControllers();

app.Run();
