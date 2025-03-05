## Classes

The following are important classes in the `HotelApp` program. You can review these with your student so they have an understanding of the program. All of the lecture programs for upcoming lectures use a similar structure.

* `Program` - The entry point of the console application. It's a best practice for this to be concise. The `Main()` method creates and instance of `HotelApp` and starts its `Run()` loop which prompts the user for menu options.
* `HotelApp` - This class prompts the user for menu options, then responds to the users choice by calling appropriate API methods (in `HotelApiService`) and methods to display results (in `HotelConsoleService`).
* `Services/HotelApiService` - In the `/Services` folder. All calls to external APIs are implemented here. This is the class in which you'll do most of your work. Method headers have been created and stubbed out as a starting point.
* `Services/HotelConsoleService` - This class is responsible for all user input and output. Remind the students that it's a good idea to separate the program's user interface from other business logic. This class contains all the UI. `HotelConsoleService` derives from `ConsoleService`.
* `Services/ConsoleService` - This is a base-class for console UI. It contains generic methods to display error and success messages, pause and wait for the user to press a key, and prompt the user for a string, an integer, or a date. The lecture code in the next several lessons use the same `ConsoleService` class.
* `Models` - The classes in the `Models` folder represent business data in the `HotelApp` program.

## Class Members

You can either implement each method and then start the application, or show the output of each change by restarting the application after each change.

Explain that `RestClient` is used to make HTTP requests and deserialize the returned JSON responses into objects. To make the request, you need a Web API endpoint URL.

Show the use of `RestClient` and the endpoint URL by opening `Services/HotelApiService.cs`:

```csharp
protected static RestClient client = null;
public HotelApiService(string apiUrl)
{
    if (client == null)
    {
        client = new RestClient(apiUrl);
    }
}
```

An instance of `RestClient` is created in the constructor. When creating a `RestClient`, you can pass a "base URL" to it. The endpoints of all requests made on this client are relative to this base URL. `client` is static so that only one instance is ever created, and the base URL isn't lost.

## Implement GET requests

Implement the code to make the _List hotels_ menu option work. In `HotelApp.cs`, `ShowHotels()` is already implemented:

```csharp
private void ShowHotels()
{
    try
    {
        List<Hotel> hotels = hotelApiService.GetHotels();
        if (hotels != null)
        {
            console.PrintHotels(hotels);
        }
    }
    catch (Exception ex)
    {
        console.PrintError(ex.Message);
    }
    console.Pause();
}

```

All methods in `HotelApp` are structured like this, so take the time to explain the first one:

* Logic is wrapped in a `try...catch`.
* First, the API service is called to retrieve data from the server.
* Next, the console service is called to display results to the user.
* If there is an error, the user is informed, using the console service.

### GetHotels()

Open `Services/HotelApiService.cs` and find the `GetHotels()` method. Replace the stub code with the following:

```csharp
public List<Hotel> GetHotels()
{
    RestRequest request = new RestRequest("hotels");
    IRestResponse<List<Hotel>> response = client.Get<List<Hotel>>(request);
    if (!response.IsSuccessful)
    {
        throw new HttpRequestException($"There was an error in the call to the server");
    }
    return response.Data;
}
```

> Note: For now, just check `response.IsSuccessful` for an error. The next lesson discusses HTTP error handling in more detail, and you can update these methods then.

Notice that since the _base URL_ has already been set in the client, only `"hotels"` is required to get to the `https://localhost:3000/hotels` endpoint.

Start the server if it's not running, run the `HotelApp` program, and select option 1 to see a list of hotels.

Now implement the remaining API calls as follows.

### GetReviews()

```csharp
public List<Review> GetReviews()
{
    RestRequest request = new RestRequest("reviews");
    IRestResponse<List<Review>> response = client.Get<List<Review>>(request);
    if (!response.IsSuccessful)
    {
        throw new HttpRequestException($"There was an error in the call to the server");
    }
    return response.Data;
}
```

### Optional: Factor the error handling

The code you use to check for errors is identical in the prior two methods, and all of the following methods. You may point out the the students that to avoid repeating yourself, you're going to factor out a method to check for errors:

```csharp
private void CheckForError(IRestResponse response)
{
    if (!response.IsSuccessful)
    {
        throw new HttpRequestException($"There was an error in the call to the server");
    }
}
```

Then you can use one line of code in all methods to check for errors. If the method returns, the calling method can return results:

```csharp
public List<Review> GetReviews()
{
    RestRequest request = new RestRequest("reviews");
    IRestResponse<List<Review>> response = client.Get<List<Review>>(request);
	CheckForError(response);
    return response.Data;
}
```

### GetHotel()

```csharp
public Hotel GetHotel(int hotelId)
{
    RestRequest request = new RestRequest($"hotels/{hotelId}");
    IRestResponse<Hotel> response = client.Get<Hotel>(request);
    if (!response.IsSuccessful)
    {
        throw new HttpRequestException($"There was an error in the call to the server");
    }
    return response.Data;
}
```

Notice the use of string interpolation to append the hotel Id to the endpoint.

### GetHotelReviews()

Next, introduce the idea that URL paths can be used to show relationships. The next example requests reviews related to a specific hotel:

```csharp
public List<Review> GetHotelReviews(int hotelId)
{
    RestRequest request = new RestRequest($"hotels/{hotelId}/reviews");
    IRestResponse<List<Review>> response = client.Get<List<Review>>(request);
    if (!response.IsSuccessful)
    {
        throw new HttpRequestException($"There was an error in the call to the server");
    }
    return response.Data;
}
```

### GetHotelsWithRating()

The next option presents the opportunity to discuss filtering requests with query parameters. In the next method, query for a filtered list of hotels that have a star rating equal to the parameter:

```csharp
public List<Hotel> GetHotelsWithRating(int starRating)
{
    RestRequest request = new RestRequest($"hotels?stars={starRating}");
    IRestResponse<List<Hotel>> response = client.Get<List<Hotel>>(request);
    if (!response.IsSuccessful)
    {
        throw new HttpRequestException($"There was an error in the call to the server");
    }
    return response.Data;
}
```

### GetPublicAPIQuery()

Finally, the last menu option allows you to revisit the public API. There's a `City` class in the `Data` folder that captures a small portion of the city response.

In the method named `GetPublicAPIQuery()`, add the following code:

```csharp
public City GetPublicAPIQuery()
{
    RestRequest request = new RestRequest("https://api.teleport.org/api/cities/geonameid:5128581/");
    IRestResponse<City> response = client.Get<City>(request);

    if (!response.IsSuccessful)
    {
        throw new HttpRequestException($"There was an error in the call to the server");
    }
    return response.Data;
}
```

Notice that because you used an _absolute_ URL, the _base URL_ property on the `RestClient` is ignored by `RestSharp`.

Modify the custom query option as much as desired.
