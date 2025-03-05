/***************************************************************
 This is service

 A service is a set of processing to facilitate the retrieval
 and storage of data - typically in a persistent source (saved)

 May do other processing as well

 This service DOES not save the data to a persistent source

 The data from this service is just an array in memory

*******************************************************************/

import { Injectable}     from '@angular/core'; // Access Angular Dependency Injection
import { MoviesInfo }    from './moviesInfo';  // Using the MoviesInfo interface
import { lastValueFrom } from 'rxjs';
import { HttpClient }    from '@angular/common/http';
import { HttpHeaders }   from '@angular/common/http';

// @Injectable tells Angular that this service may be Dependency Injection
@Injectable({
  providedIn: 'root'
})

export class MoviesService {

  // A module contains data and methods/functions for processing that data

  // Source of data for calls to retrieve the data - initialized in the code
  private listOfMovies : any[] = []  // An array of MovieInfo objects

  // Set up a string to hold teh root URL/endpoint for the server
  private theServerURL : string = "https://localhost:7223/api/MovieApi/movies"

  // a constructor is method to initialized data defined in the module
  // it is executed automatically when the service is loaded
  // Here we use the constructor to initialize our data source
  // Receive an HttpCLient object to Access the external import
  // The HttpClient object will be Dependency Injected into the constructor
  //     and automatically assigned to the name used as a parameter
  // theServer variable will be set to an HttpClient object automagically 
  constructor(private theServer:HttpClient) {}
  
  // methods others may use to interact with our service

  // This method will return the current data in our data source (an external API)
  // We made this async because it will do an async call to the external API
      async getMoviesList()  : Promise<any[]> {  // this function returns a MoviesInfo array

        // Make the call to the external API and store in our local data source (array)
        // lastValueOf() converts the Observable object returned from the http get()
        //               to a promise

        const result : any[] = await lastValueFrom(this.theServer.get<any[]>(this.theServerURL))

        this.listOfMovies = result;


        return this.listOfMovies;
      }
  // This method will receive a MoviesInfo object and add it to our data source (listOfMovies)
      async addMovies(newMovie : MoviesInfo) {      
          // Create a header to tell the server we are sending it JSON data with the request
      // 'application/json' is how you indicate JSON data
      // the Angular HttpCLient takes care of converting the data for you, if necessary
      // 'Content-Type' is the attribute where type of incoming data is specified
      // Use new HttpHeaders too create a JSON object with the attributes you want to send
      const headers = new HttpHeaders ({
        'Content-Type' : 'application/json'
       });
       
      // use http.post(API-URL, data-to-send, {header-object})                               
      return lastValueFrom(this.theServer.post(this.theServerURL+"/create", newMovie, {headers}));

      }

  // This method will delete a movie from our data source
       async deleteMovie(aMovie : MoviesInfo) {
 //       console.log("Got to delete movie")
 //       console.table(aMovie)
       await lastValueFrom(this.theServer.delete(this.theServerURL + "/delete/"+aMovie.movieId))
      }    
    
// Note: this option has not been implemented in the component  
// This method will update a movie
    async updateMovie(aMovie : MoviesInfo) {

    }    
}
