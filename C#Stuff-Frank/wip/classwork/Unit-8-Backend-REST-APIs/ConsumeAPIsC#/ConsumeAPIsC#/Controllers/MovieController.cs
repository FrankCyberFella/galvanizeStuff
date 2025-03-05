using ConsumeApisC_.Services;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using moviesAPI.Models;

namespace ConsumeApisC_.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class MovieController : ControllerBase
    {
        // Define a reference for access to MovieService methods
        private readonly MovieService _movieService;

        // Have ASP.NET Depenedency Inject service to the class
        public MovieController(MovieService movieService)
        {
            // Assign the D.I. service object to our reference for it
            this._movieService = movieService;
        }

        // Controller to retreieve and return all movies from MovieService
        [HttpGet("movies")]
        public Task<List<Movie>> GetMovies()
        {
            // AsyncCallback the MovieService to retrieve all the movies and return them
            return _movieService.AllTheMovies();

        }
        [HttpPost("movies/add")]
        public 

    }
}
