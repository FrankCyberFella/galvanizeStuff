
using moviesAPI.Models;  // Give me access to class that describes a Movie
using System.Text.Json;

namespace CallAPIsConsole
{
    internal class Program
    {
        // Since this method will be making async calls, we add teh async attribute to the method signature
        //       and indicate it may return a Task (even though we don't in thsi example)
//      static void Main(String[] args) originally
        static async Task Main(string[] args)
        {
           // This code will make Http REST requests to our Movies API Server
           // So this is considered a Client to the API server

           // Define an HttpClient object to issue the Http REST requests
           HttpClient theClient = new HttpClient();

            // Set the Base URI for accessing the server
            // The Base URI is the startof the URI for accessing the server
            // We just add the path to the Base URI when we make out API class
            theClient.BaseAddress = new Uri("https://localhost:7223/api/MovieApi/");

            // Hold all Movies from teh datasource for processing
            List<Movie> allTheMovies = null;  

            /********************************************************************************************
             * HTTP Get Example
             *******************************************************************************************/
            // Call API server to get a list of allthe movies in its data source

            Console.WriteLine("\n---- Retrieving all data from Data Source via GET ----");

            // Define a HttpResponseMessage object to hold the respone from the API call
            // To make the call to the API:  HttpClient.GetAsync("path")
            // The await attribute tells C# to wait for the API call to return before continuing
            HttpResponseMessage theResponse = await theClient.GetAsync("movies");

            // If the API call was successful, get the data from the response into objects
            //    so the program can process it
            // Data is returned as JSON from the server
            if (theResponse.IsSuccessStatusCode)  
            {
                String dataReturned = await theResponse.Content.ReadAsStringAsync(); // Get the data from the response
                Console.WriteLine(dataReturned);
                // Convert the JSON from the response into object(s) we can use
                // JsonSerializer will convert JSON to C# objects
                //
                // Serialize - convert to an object to a String
                // Deserialize - convert a String to an object

                // Since multiple JSON objects are returning from the API 
                // and assign to data structure we have to gold them
                allTheMovies = JsonSerializer.Deserialize<List<Movie>>(dataReturned);

                foreach(Movie aMovie in allTheMovies)
                {
                    Console.WriteLine(aMovie.Title);
                }
            }
            else
            {
                Console.WriteLine("Error in API call: " + theResponse.StatusCode);
            }

            /********************************************************************************************
            * HTTP Post Example
            *******************************************************************************************/
            Console.WriteLine("\n---- Adding to Data Source via POST ----");

            // Do an HTTP Post to add a movie

            // Define a new Movie object to add to the API data source
            // Since the Movie class does not have cnstructors defined,
            //     we will define a default Movie and use properties to populate it

            Movie newMovie = new Movie();  // Define an empty Movie
            newMovie.Title = "Nestor the Long Christmas Donkey";
            newMovie.ReleaseYear = 1972;
            newMovie.Director = "Bankin/Rask";

            // Convert object to JSON
            String jsonForObject = JsonSerializer.Serialize(newMovie);

            // Convert JSON to StringContent needed for Http Request 
            StringContent theData = new StringContent(jsonForObject, System.Text.Encoding.UTF8, "application/json");

            // Define a HttpResponseMessage object to hold the respone from the API call
            // To make the call to the API:  HttpClient.PostAsync("path", string-content-object)
            // The await attribute tells C# to wait for the API call to return before continuing
            HttpResponseMessage thePostResponse = await theClient.PostAsync("movies/create", theData);

            // Get data from the request
            String dataReturnedPost = await thePostResponse.Content.ReadAsStringAsync(); // Get the data from the response
            Console.WriteLine(dataReturnedPost);

            Movie theMovie = JsonSerializer.Deserialize<Movie>(dataReturnedPost);

            Console.WriteLine(theMovie.Title);

         /********************************************************************************************
          * HTTP Put Example
          *******************************************************************************************/
            Console.WriteLine("\n---- Updating Data Source via PUT ----");

            // Do an HTTP Put to update a movie

            // Retrieve Movie from data source to be updated 
            Movie movieToUpdate = allTheMovies[0]; 

            Console.WriteLine("Updating: " + movieToUpdate.Title);

            // Change one or more data values in the movie to be updated
            movieToUpdate.Director = "Frank";

            // Convert object to JSON
            String jsonForUpdateObject = JsonSerializer.Serialize(movieToUpdate);

            // Convert JSON to StringContent needed for Http Request 
            StringContent theUpdateData = new StringContent(jsonForUpdateObject, System.Text.Encoding.UTF8, "application/json");

            // Define a HttpResponseMessage object to hold the respone from the API call
            // To make the call to the API:  HttpClient.PutAsync("path", string-content-object)
            // The await attribute tells C# to wait for the API call to return before continuing'
            // Note:  This particular API Put does not return anything but the Http Status Code 
            HttpResponseMessage thePutResponse = await theClient.PutAsync("movies/update", theUpdateData);

            // Get data from the request
            Console.WriteLine("Http Status from Put: " + thePutResponse.StatusCode);

        /********************************************************************************************
         * HTTP Delete Example
         *******************************************************************************************/
            Console.WriteLine("\n---- Delete a instance from Data Source via DELETE ----");

            // Do an HTTP Put to update a movie

            // Set MovieId to be deleted 
            int movieIdToDelete = 5; ;

            Console.WriteLine("Deleting movieId: " + movieIdToDelete);

            // Define a HttpResponseMessage object to hold the respone from the API call
            // To make the call to the API:  HttpClient.PutAsync("path", string-content-object)
            // The await attribute tells C# to wait for the API call to return before continuing
            // Note:  This particular API Delete does not return anything but the Http Status Code 
            HttpResponseMessage theDeleteResponse = await theClient.DeleteAsync("movies/delete/" + movieIdToDelete);

            // Get data from the request
            Console.WriteLine("Http Status from Delete: " + theDeleteResponse.StatusCode);

        } // End of Main()
    }  // End of class Program
} // End of namespace
