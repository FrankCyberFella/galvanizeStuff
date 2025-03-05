
using DoctorApp.Models;
using Microsoft.EntityFrameworkCore;

namespace DoctorApp
{
    public class Program
    {
        public static void Main(string[] args)
        {
            // WebApplication is a container for eveything you're using in the app
            var builder = WebApplication.CreateBuilder(args);

            // Add services to the container.
            builder.Services.AddDbContext<DoctorDbContext>(option =>
                    option.UseSqlServer(builder.Configuration.GetConnectionString("DoctorDB")));

            builder.Services.AddControllers();
            // Learn more about configuring Swagger/OpenAPI at https://aka.ms/aspnetcore/swashbuckle
            builder.Services.AddEndpointsApiExplorer();
            builder.Services.AddSwaggerGen();

            var app = builder.Build();

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
        }
    }
}
