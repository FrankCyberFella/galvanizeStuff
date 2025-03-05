# Server Side APIs: Part 1 - Lecture Notes

## Preparing for lecture

The lecture project is configured to use HTTPS because it's considered best practice to use HTTPS in development if HTTPS is used in production. The lecture project is also configured to run in IIS Express, so the development HTTPS certificate is trusted. The tutorial used IIS Express, so the students are likely to be familiar with it.

**Note**: The Postman screenshots use HTTP and port 8080; however, the results of each request should be the same, though with different dates.

## Running the application

To run this application, open `ServersideAPIsPart1Lecture.sln` in Visual Studio and run it using IIS Express. You'll test the API in Postman, and connect a client application as the last step. The client application is the end result of the previous day's lecture.

## Student starting code

This lecture continues creating the Hotel application. The client application is virtually identical to the one built in previous lessons. The only change is that instead of referencing an API at `http://localhost:3000`, it's now using HTTPS and a different port. That's because the goal today is to replace the mock JSON-server with a working ASP.NET MVC API server.

The project in the `server` folder is the MVC server. The project has some endpoints implemented already. You'll implement the following endpoints in class:

- List all reservations in the system
- Get a reservation by its ID
- List all reservations by a hotel
- Create a new reservation
- Filter hotels by city and state

## Lecture code walkthrough

**Overview**

- Project Walkthrough
  - MVC Structure
  - Models
    - Hotel
    - Address
    - Reservation
  - DAOs
- ReservationsController
  - GET
    - all reservations
    - single reservation by ID
    - all reservations by hotel
  - POST
    - create new reservation
- HotelsController
  - GET
    - Filter hotels by state and city
- Connect the client to the API

### Project walkthrough

Before you write any code, walk through the structure of a ASP.NET MVC Web API project and answer any questions the students might have.

*Note:* In August 2022 Microsoft rebranded "ASP.NET Core" back to the original "ASP.NET" 

There's some existing code that you can walk through before you create methods in the controller:

- `Models/Hotel.cs`: Represents a Hotel in the API
- `Models/Address.cs`: Represents an Address in the API
- `Models/Reservation.cs`: Represents a Reservation in the API
- `DAO/IHotelDao.cs`: Interface that all hotel dao objects must implement
- `DAO/HotelMemoryDao.cs`: A class that handles in-memory storage and hotel data access
- `DAO/IReservationDao.cs`: Interface that all reservation dao objects must implement
- `DAO/ReservationMemoryDao.cs`: A class that handles in-memory storage and reservation data access

## Controllers

In the application, you'll find the "Controllers" folder that contains `HotelsController.cs` and `ReservationsController.cs`. Open `HotelsController.cs` to show the controller class:

```csharp
namespace HotelReservations.Controllers
{
    [Route("hotels")]
    [ApiController]
    public class HotelsController : ControllerBase
    {
        private static IHotelDao hotelDao;

        public HotelsController()
        {
            if (hotelDao == null)
            {
                hotelDao = new HotelMemoryDao();
            }
        }

    // ...

    }
}
```

Walk through what the constructor does. Students haven't learned about dependency injection yet, so for the time being, the controller just creates a new instance of the DAO.

`HotelsController` already contains two endpoints:

* `GET /hotels` - implemented in `ListHotels()`
* `GET /hotels/{id}` - implemented in `GetHotel()`

`ReservationsController` contains a constructor, but there are no endpoints implemented yet.

### Testing the API with Postman

This is a great opportunity to show the students good practices when it comes to building APIs. A careful programmer won't worry about hooking up the front-end code until they know their API does what they want.

You can take the opportunity to use tools like Postman, Chrome Developer Tools (Network Tab), and debugger break points to step through the process of handling a request.

Run the application and test the existing endpoints:

- `GET`: `/hotels`
- `GET`: `/hotels/{id}` (1-7)

![List Hotels](./img/list_hotels.png)

### Adding new methods

At this point, you can map out on the board what methods you need in your controller based on the requirements that the front end team gave to you:

- list all reservations
  - path: `/reservations`
  - request method: `GET`
  - return: list of all reservations in the system
- get reservation by ID
  - path: `/reservations/{id}`
  - request method: `GET`
  - return: reservation info for given ID using path variable
- list all reservations by hotel
  - path: `/hotels/{id}/reservations`
  - request method: `GET`
  - return: list of all reservations in the system by hotel
- add new reservation
  - path: `/reservations`
  - request method: `POST`
  - add a new reservation to the given hotel based on the request body
- filter hotels
  - path: `/hotels?state={state}&city={city}`
  - request method: `GET`
  - find hotels by state and city (optional)

You'll implement the first four endpoints in `ReservationsController`, and the final one in `HotelsController`.

#### List reservations

The first method you'll write returns all the reservations in the list. In `ReservationsController.cs`, add:

```csharp
[HttpGet()]
public List<Reservation> ListReservations()
{
    return reservationDao.GetReservations();
}
```

Note that since the `Route` attribute at the top of the class is "reservations", and there's no additional route in the `HttpGet` attribute, the endpoint is `GET /reservations`.

![List Reservations](./img/list_reservations.png)

#### Get a reservation by ID

Next, you need the ability to get a single reservation using the reservation ID. The first thing to point out is the use of a path variable—`{id}`. The reservation ID is a `int`, so you'll map whatever comes through in the path to an `int` named `id`. The complete endpoint is `GET /reservations/{id}`:

```csharp
[HttpGet("{id}")]
public ActionResult<Reservation> GetReservation(int id)
{
    Reservation reservation = reservationDao.GetReservationById(id);

    if (reservation != null)
    {
        return reservation;
    }
    else
    {
        return NotFound();
    }
}
```

Also notice the use of `ActionResult` as a return type, and the possibility of returning `NotFound()` which translates to HTTP status code `404`. When you test this endpoint in Postman, try with an ID that exists, and also with an ID that doesn't exist.

![Get Reservation](./img/get_reservation.png)

#### List reservations by hotel

Next, you'll list all reservations by hotel. This is similar to the previous method, but with one difference: the path variable doesn't always need to be `id`. It can be something different as long as it's the same between the path template (`hotels/{hotelId}/reservations`) and the method parameter (`int hotelId`):

```csharp
[HttpGet("/hotels/{hotelId}/reservations")]
public ActionResult<List<Reservation>> ListReservationsByHotel(int hotelId)
{
  Hotel hotel = hotelDao.GetHotelById(hotelId);
  if (hotel == null)
  {
    return NotFound();
  }
  return reservationDao.GetReservationsByHotel(hotelId);
}
```

Note the use of the slash `/` to start the endpoint name in the `HttpGet` attribute. This is an absolute endpoint name, so it overrides the class's default `Route` of `/reservations`.

![List Reservations by Hotel](./img/list_reservations_by_hotel.png)

#### Add a reservation

Last but not least, there's a way to add a new reservation in the application. Be sure to point out that the `reservation` parameter takes the JSON body and converts it to a `Reservation` object:

```csharp
[HttpPost()]
public ActionResult<Reservation> AddReservation(Reservation reservation)
{
    Reservation added = reservationDao.CreateReservation(reservation);
    return Created($"/reservations/{added.Id}", added);
}
```

This endpoint is `POST /reservations`. Notice this method returns `Created`, which is HTTP status code `201`.

![Create Reservation](./img/create_reservation.png)

#### Filter hotels by city and state

Show students that they can modify the `ListHotels` method so that it filters results by city and state. The modified method is a good example of how to use query string parameters—they don't have to be defined in the route template, but they're automatically mapped to method parameters. If any are omitted, they're `null`. 

Open `HotelsController.cs` and modify `ListHotels()` so that it accepts the parameters `state` and `city`:

```csharp
[HttpGet()]
public List<Hotel> ListHotels(string state = "", string city = "")
```

Change the `ListHotels()` method to pass the `state` and `city` parameters to a call to `hotelDao.GetHotelsByStateAndCity()`:

```csharp
[HttpGet()]
public List<Hotel> ListHotels(string state = "", string city = "")
{
  return hotelDao.GetHotelsByStateAndCity(state, city);
}
```

### Connecting the client application

The client application from the previous lecture is located in `dotnet/client`. The API that you built out today modeled the fake API that was used. The only code that changed was the URL of the API in `Program.cs`:

```csharp
class Program
{
    private const string ApiUrl = "https://localhost:44322/";

    // ...
}
```

If the API that you worked on today is running, run the command-line application and verify that everything works.

> The previous client application has the ability to update and delete a reservation which is covered in a future lecture. The client also doesn't have the ability to filter by state and city, but that could be something the students can work on if they have time.

At this point, you'll see the two applications communicating with each other.
