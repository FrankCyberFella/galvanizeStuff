
// Instantiate and initialize builder for web applications and services.
using GamblerAPI.Model;              // Gain access to Gambler Model/Class
using Microsoft.EntityFrameworkCore; // Gain access to Entity Framework

/***************************************************************************************
 * WebApplication configures the connection/pipeline  between the app and a server
 * 
 * It uses the "Builder Design Pattern" to initialize various options
 * 
 * In the "Builder Design Pattern, an object is initialized step-by-step
 * using individual methods to set properties in the object instead of constructors
 * and then runs the "build" method to actual instantiate the
 * object using the properties previously set.
 ****************************************************************************************/ 

// Instatiate a "builder" for this WebAppliction
// args represents any configuration arguments that may have been passed to the app
//                 when executed from the command line
var builder = WebApplication.CreateBuilder(args);

// Add services to the WebApplication container

// Add DbContext for server and database used in the app
// Server Connection String is stored in and obtained from appsettings.json file
//                   it contains the name of the server and database you are using
//                   and possibly some additional options
builder.Services.AddDbContext<GamblerDbContext>(option =>
    option.UseSqlServer(builder.Configuration.GetConnectionString("vegasdb")));

// Enable CORS for the app - Who can send requests to the server (who can we share data with)
// Allow all requests from anywhere for any method for testing purposes
//     we would normally restrict which domains/methods we would accepts requests from
builder.Services.AddCors(options =>
{
    options.AddDefaultPolicy(
        builder =>
        {
            builder.AllowAnyOrigin().AllowAnyHeader().AllowAnyMethod();
        });
});

// Register everything that is needed for Web API Development.
//    This includes support for Controllers, Model Binding, API Explorer, Authorization, CORS et al
builder.Services.AddControllers();

// Learn more about configuring Swagger/OpenAPI at https://aka.ms/aspnetcore/swashbuckle
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();

// Build the WebApplication artifacts using previously established option
var app = builder.Build();

// Additional configure properties for HTTP request pipeline/connection

// Add use of Swagger if running in development environment
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

// Add app endpoints - Paths and HTTP request we have defined in the controllers
app.MapControllers();

// Add use of Cross Origin Resource Sharing to control what can access this all
// May be optional depending on other appication options
app.UseCors();  

// Start the application
//      Be sure server is up and running to avoid error 
app.Run();
