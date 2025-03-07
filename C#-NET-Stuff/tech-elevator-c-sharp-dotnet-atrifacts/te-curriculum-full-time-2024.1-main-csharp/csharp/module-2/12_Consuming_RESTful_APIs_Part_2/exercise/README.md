# Consuming APIs: POST, PUT, and DELETE (C#)

In this exercise, you'll continue working on the command-line application that displays online auction info. You'll continue to add code for other API methods.

Your task is to add web API calls using RestSharp to create new auctions (`POST`), update existing auctions (`PUT`), and delete auctions (`DELETE`).

## Step One: Start the server

Before starting, make sure the web API is up and running. Open the command line and navigate to the `./server/` folder in this exercise.

First, run the command `npm install` to install any dependencies. You won't need to do this on any subsequent run.

To start the server, run the command `npm start`. If there aren't any errors, you'll see the following, which means that you've successfully set up your web API:

```shell
  \{^_^}/ hi!

  Loading data-generation.js
  Loading middleware.js
  Done

  Resources
  http://127.0.0.1:3000/auctions

  Home
  http://127.0.0.1:3000

  Type s + enter at any time to create a snapshot of the database
```

> Remember: The name `localhost` resolves to `127.0.0.1`, so when you see that IP address in the message, it means that json-server is ready to respond to requests on `localhost`.

You can stop the server, or any other process that you've started from the console, by using the keyboard shortcut `ctrl + c`.

In this exercise, you'll modify data on the server. As you're working, you may come across a situation where you want to reset the data. To do this, first stop the server with `ctrl + c`, then restart it with `npm start`.

## Step Two: Explore the API

Before moving on to the next step, explore the web API using Postman. You can access the following endpoints:

- GET: `http://localhost:3000/auctions`
- GET: `http://localhost:3000/auctions/{id}` (use a number between 1 and 7 in place of `{id}`)

These are the endpoints you'll work on for this exercise:

- POST: `http://localhost:3000/auctions`
- PUT: `http://localhost:3000/auctions/{id}`
- DELETE: `http://localhost:3000/auctions/{id}`

## Step Three: Evaluation criteria and functional requirements

* All unit tests pass in `AuctionApp.Tests`.
* Code is clean, concise, and readable.

To complete this exercise, you need to complete the `AuctionApiService` class by implementing the `AddAuction()`, `UpdateAuction()`, and `DeleteAuction()` methods.

### Tips and tricks

* The `Auction` class has a constructor which takes five parameters that set the `Id`, `Title`, `Description`, `User`, and `CurrentBid` properties.
* The `AddAuction()` method takes an `Auction` object as a parameter that's passed from the caller. Have the `AddAuction()` method return the `Auction` object returned from the API when it's successful. Throw an `HttpRequestException` if unsuccessful.
* The `UpdateAuction()` method takes an `Auction` object as a parameter that's passed from the caller. Have the `UpdateAuction()` method return the `Auction` object returned from the API when it's successful. Throw an `HttpRequestException` if unsuccessful.
* The `DeleteAuction()` method takes an `int` as a parameter that's passed from the caller. It's the `Id` of the auction to delete. Have the `DeleteAuction()` method return `true` if successful. Throw an `HttpRequestException` if not successful.
* Consider that the server may return an error, or you might not get a response at all. Implement the necessary error handling.

## Step Four: Add a new auction

In `Services/AuctionApiService.cs`, the `AddAuction()` method creates a new auction. Make sure to handle any errors returned from the server.

When you've completed the `AddAuction()` method, run the unit tests, and verify that the `AddAuction_ExpectSuccess` and `AddAuction_ExpectExceptionForInvalidUrl` tests pass.

## Step Five: Update an existing auction

The `UpdateAuction()` method overwrites the existing auction with an updated one for a given ID. Make sure to handle any errors returned from the server.

When you've completed the `UpdateAuction()` method, run the unit tests, and verify that the `UpdateAuction_ExpectSuccess`, `UpdateAuction_ExpectExceptionForInvalidData`, and `UpdateAuction_ExpectExceptionForInvalidUrl` tests pass.

## Step Six: Delete an auction

The `DeleteAuction()` method removes an auction from the system. Make sure to handle any errors returned from the server. What happens if you enter an ID for an auction that doesn't exist?

When you've completed the `DeleteAuction()` method, run the unit tests, and verify that the `DeleteAuction_ExpectSuccess`, `DeleteAuction_ExpectExceptionForInvalidData`, and `DeleteAuction_ExpectExceptionForInvalidUrl` tests pass.

Once all unit tests pass, you've completed this exercise.
