# C# Lecture Notes

## Add reservation

Open `HotelApp.cs` and look at menu option #3. This is where a user can add a new reservation.

The user must first select a hotel for the reservation. The console service method `PromptForHotelId()` displays a list of hotels and asks the user to select one. It returns the `Id` of the selected hotel. You must first call the `GetHotels()` API to get a list of hotels to pass to the `PromptForHotelId()` method.

Once the user selects a hotel, `PromptForReservationData()` collects reservation details from the user, and returns a `Reservation` object. Finally, calling `AddReservation()` on the API service sends the data to the server to create a reservation.

You must implement the `AddReservation()` method. Open `Services/HotelApiService.cs` and find the `AddReservation()` method. Replace its contents:

```csharp
public Reservation AddReservation(Reservation newReservation)
{
    RestRequest request = new RestRequest("reservations");
    request.AddJsonBody(newReservation);
    IRestResponse<Reservation> response = client.Post<Reservation>(request);
    CheckForError(response, "Add reservation");
    return response.Data;
}
```

Point out the differences between getting and posting data. For the post, you serialize the object into the body of the request using `AddJsonBody()`, and you call the `Post()` method on the client.

### Extend error handling

In the previous lesson, you factored the error checking into a separate method:

```csharp
private void CheckForError(IRestResponse response, string action)
{
    if (!response.IsSuccessful)
    {
        // TODO: Write a log message for future reference
        throw new HttpRequestException($"There was an error in the call to the server");
    }
}
```

Now you'll extend the error checking. Checking `response.ResponseStatus != ResponseStatus.Completed` catches an error that might occur if the server can't be reached for reasons like network issues or DNS. Checking `!response.IsSuccessful` catches HTTP exceptions that include status codes, like 401 (Unauthorized), 404 (Not Found), or a general server exception (500). Update the `CheckForError()` method to whatever extent you'd like, but make sure you check `ResponseStatus` and `IsSuccessful` at a minimum. The following method also includes logic to log more information into a log file. The class `BasicLogger` is included in the project for you to use:

```csharp
private void CheckForError(IRestResponse response, string action)
{
    string message = "";
    string messageDetails = "";
    if (response.ResponseStatus != ResponseStatus.Completed)
    {
        message = $"Error occurred in '{action}' - unable to reach server.";
        messageDetails = $"Action: {action}\n" +
            $"\tResponse status was '{response.ResponseStatus}'.";
        if (response.ErrorException != null)
        {
            messageDetails += $"\n\t{response.ErrorException.Message}";
        }
    }
    else if (!response.IsSuccessful)
    {
        message = $"An http error occurred.";
        messageDetails = $"Action: {action}\n" +
            $"\tResponse: {(int)response.StatusCode} {response.StatusDescription}";
    }
    if (message.Length > 0)
    {
        BasicLogger.Log($"{message}\n\t{messageDetails}\n");
        throw new HttpRequestException(message, response.ErrorException);
    }
}
```

Because you took the time to factor out the error checking, you only need to make this change in one place to take advantage of it from all server calls.

After you've completed this method, run the application, add a new reservation to an existing hotel, and verify that it was added. You might also want to show the exception handling here. Try doing something with the server stopped.

## Update reservation

Menu option #4 is where a user can update an existing reservation. You can review the appropriate code in `HotelApp.cs` and `HotelConsoleService.cs`, but you'll make changes in `HotelApiService`. Updating a reservation is similar to adding a new one but with two changes. You use the `Put()` method of the `RestClient`, and you need to pass the `Id` of the reservation you're changing as part of the URL:

```csharp
public Reservation UpdateReservation(Reservation reservationToUpdate)
{
    RestRequest request = new RestRequest($"reservations/{reservationToUpdate.Id}");
    request.AddJsonBody(reservationToUpdate);
    IRestResponse<Reservation> response = client.Put<Reservation>(request);
    CheckForError(response, $"Update reservation {reservationToUpdate.Id}");
    return response.Data;
}
```

After you've completed the method, run the application, update an existing reservation, and verify that the changes were saved.

## Delete reservation

The `DELETE` method differs from the `POST` and `PUT` methods in two ways. First, you don't need to return anything back to the client. Second, you don't need to use the `AddJsonBody()` method because you aren't sending anything in the request body.

To perform a `DELETE` request, use the `Delete()` method of the `RestClient`. Like the `PUT` operation, you'll pass the `Id` of the reservation as part of the path in the URL:

```csharp
public bool DeleteReservation(int reservationId)
{
    RestRequest request = new RestRequest($"reservations/{reservationId}");
    IRestResponse response = client.Delete(request);
    CheckForError(response, $"Delete reservation {reservationId}");
    return true;
}
```

After you've completed the method, run the application, delete an existing reservation, and verify that the changes were saved. You can try and delete a reservation with an ID that doesn't exist and see the 404 status error being thrown.
