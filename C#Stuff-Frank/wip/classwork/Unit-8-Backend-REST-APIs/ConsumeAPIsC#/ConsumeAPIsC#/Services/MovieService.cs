using Microsoft.AspNetCore.Mvc;
using moviesAPI.Models;
using System.Net.Sockets;
using System.Text.Json;

namespace ConsumeApisC_.Services
{
    public class MovieService
    {
        // Define an object to interact with the server
        private readonly HttpClient _httpClient;

        // Constructor to initialize any data
        public MovieService(HttpClient httpClient)
        {
            _httpClient = httpClient;
            _httpClient.BaseAddress = new Uri("https://localhost:7223/api/MovieApi/");
        }
        // Method to get all the Movies from the MovieAPI data source
        public async Task<List<Movie>> AllTheMovies()
        {
            // Use the HttpCLient object to go to the server and retrieve the data
            HttpResponseMessage theResponse = await _httpClient.GetAsync("movies");

            // Get the JSON data from the respone
            String dataReturned = await theResponse.Content.ReadAsStringAsync();

            // Convert the JSON from the response to a MOvie Object
            // JsonSerializer.Deserialize<result-data-type>(JSON-Data)
            return JsonSerializer.Deserialize<List<Movie>>(dataReturned);
        }
        // Method to add a Movie to the data source

        public async Task<Movie> addMovie(Movie newMovie)
        {

            // Convert object to JSON
            String jsonForObject = JsonSerializer.Serialize(newMovie);

            // Convert JSON to StringContent needed for Http Request 
            StringContent theData = new StringContent(jsonForObject, System.Text.Encoding.UTF8, "application/json");

            // Define a HttpResponseMessage object to hold the respone from the API call
            HttpResponseMessage thePostResponse = await _httpClient.PostAsync("movies/create", theData);

            // Get data from the request
            String dataReturnedPost = await thePostResponse.Content.ReadAsStringAsync(); // Get the data from the response
                                                                                         // Return data from request
            return JsonSerializer.Deserialize<Movie>(dataReturnedPost);
        }
    }
}
