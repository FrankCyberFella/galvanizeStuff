# Lecture code walkthrough

This lesson extends the previous day's hotel listing example and adds features for making reservations.

## Hotel reservations application

You're welcome to start the lecture by walking around the application and showing the differences between this application and the previous one. The previous application focused on hotels and reviews while this one allows customers to make reservations.

There is now a `Reservation` model which has some basic properties that make up a reservation. The command line and API calls have been refactored into their own classes.

### Start the server

It's important for students to get into the habit of testing an API using Postman, or a similar tool. So before you write any code, open up a terminal in the `server` folder. Then execute the commands `npm install` and `npm start`. The application starts up and is available at http://localhost:3000/.

The console contains:

```shell
  \{^_^}/ hi!

  Loading hotel-reservations.json
  Loading routes.json
  Loading middleware.js
  Done

  Resources
  http://127.0.0.1:3000/hotels
  http://127.0.0.1:3000/reservations

  Other routes
  /hotels/:id/reservations -> /reservations?hotelId=:id

  Home
  http://127.0.0.1:3000

  Type s + enter at any time to create a snapshot of the database
```

In this lecture, you'll modify data on the server. As you're working, you or a student may come across a situation where you want to reset the data. To do this, first stop the server with `Ctrl+c`, then restart it with `npm start`.

Open Postman and look at the existing endpoints:

- GET http://localhost:3000/hotels
- GET http://localhost:3000/hotels/{id}
- GET http://localhost:3000/reservations
- GET http://localhost:3000/reservations/{id}
- GET http://localhost:3000/hotels/{id}/reservations

These are the endpoints you'll write code for today. Take some time to use Postman to review these endpoints. This might be the first time they've used Postman to create something other than a `GET` request:

- POST http://localhost:3000/reservations
- PUT http://localhost:3000/reservations/{id}
- DELETE http://localhost:3000/reservations/{id}


## POST, PUT, and DELETE reservations

The rest of this lecture focuses on creating the methods to add, update, and delete reservations.

[C# Lecture Notes](./lecture-code-dotnet.md)
