using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using moviesAPI.Models;
using System.Reflection.Metadata.Ecma335;

namespace moviesAPI.Controllers
{
    // Sets the base URL for API calls to the controller
    // if [controller] annotation is included it adds controller name to the path
    [Route("api/[controller]")]
    [ApiController]   // This file contrains API controllers
    public class MovieApiController : ControllerBase
    {
        // This controller will use Entity Framework to access the data in movie table
        // We need a DbContect object that represents the table to Entity Framework

        // Define a reference to a DbContext object for the table
        private readonly MoviedbContext _context;

        // Constructor that assigns the Dependeny Injected DbContext object to our reference
        public MovieApiController(MoviedbContext context)
        {
            _context = context;
        }

        // Methods to handle various HTTP Requests and URL paths
        // Retrieve all the rows - more than one row may be returned, so we return a List
        [HttpGet("movies")]
        //           Task<return-type>  name-of-method(parameters)
        public async Task<List<Movie>> getMovies()
        {
            // call Entity Frame to get all the movies and return them
            return await _context.Movies.ToListAsync();
        }

        // Add a movie to the data source
        [HttpPost("movies/create")]  // It's ok to reuse path as long as the HTTP request is different
        // Data to be added to the data source will be passed in the body of the HTTP POST request as JSON
        // [Bind(list-of-class-variables)]  Tells Entity Framework which class valiables to use
        //                                  when creating the object being passed as a parameter
        //                                  from the JSON sent to the server
        public async Task<Movie> blockBuster([Bind("MovieId, Title, ReleaseDate, Director")] Movie newMovie)
        {
            // context.operation(data)
            _context.Add(newMovie);               // Data is added to a memory of the table
            await _context.SaveChangesAsync();    // Call Entity Framework to save new data in memory to data source

            return newMovie;
        }

        // Delete a movie based in id
        // Use a parameter variable in  the URL to get the key we want to delete
        [HttpDelete("movies/delete/{id}")] // This will handle /movies/delete/id
        public async Task<ActionResult<int>> dropTheGunGetTheCannoli(int id)
        {
            var aMovie = await _context.Movies.FindAsync(id); // Call Entity Framework to see if movie to be delete exists in the data source

            if (aMovie != null)  // If tehre is a Movie in the data source
            {
                _context.Movies.Remove(aMovie);                   // Call Entity Framework to mark it as removed it from the data soure
                _context.SaveChanges();                           // Save/Commit the delete in tehdata source
            }
            return Ok();
        }

        // Update a Movie
        [HttpPut("movies/update")]
        public async Task<ActionResult<int>> changeMovieInfo([Bind("MovieId, Title, ReleaseDate, Director")] Movie updatedMovie)
        {
            try                                     // Attempt to...
            {
                _context.Update(updatedMovie);      //     Call Entity Framework to update data in data source
                await _context.SaveChangesAsync();  //     Call Entity Framework to save/commit updated data in data source
            }
            catch (DbUpdateConcurrencyException)    // If unexpected data base error occurs..
            {
                if (MovieExists(updatedMovie.MovieId))    //      and update item exists...
                {
                    throw;                          //          throw the exception to next process in call stack
                }
                else                                //      and update item does not exist...
                {
                    return NotFound();              //          return with Not Found (404) HTPP status code
                }
            }
            return Ok();
          
        }
    
    
     // Helper method to determine is a particular task exists in the data source
        private bool MovieExists(int id)
        {
            // Call Entity Framework to determine is there is any gambler in data source with id passed
            //      and return what it returns (true if there is, false if there is not)
            return (_context.Movies?.Any(e => e.MovieId == id)).GetValueOrDefault();
        }
    }

}
