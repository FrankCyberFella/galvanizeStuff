# Authentication Lecture

## Overview of session

In this lesson, you'll continue working with the hotel reservations example. Now that you've replaced the fake API server with a real one, you'll add authentication and authorization to the client and server applications.

The server application has been modified to use JWT authentication and provide a login endpoint. The client application has been modified to provide an option to log in.

## Session objectives

- Define and differentiate between the terms "authentication" and "authorization" as they pertain to a client-server or Web application
- Describe the general mechanics and workflow of how JSON Web Tokens (JWTs) are used to authenticate users of a client-server (including Web) application
- Use a common tool to decode an encoded JWT to inspect its contents
- Recognize and interpret the HTTP response status codes commonly associated with authentication and authorization failures, like 401 and 403
- Write client code in Java/C# that can authenticate with an authentication server to retrieve a JWT, and then use the JWT to authenticate subsequent requests to a Web API.
- Utilize the authorization features of an application framework (Spring Boot/ASP.NET Web API) to:
  - Specify that a particular resource requires authentication to be accessed
  - Specify that a particular resource can be accessed anonymously
  - Apply simple authorization rules for resources
  - Obtain the identity of an authenticated user

## Instructor notes

- [.NET Lecture Code Walkthrough](./dotnet-lecture-code.md)

## Instructor references

[Intro to JWT](https://jwt.io/introduction/)

[JWT decoder](https://jwt.io/#debugger-io)

.NET
- [Simple authorization in ASP.NET](https://learn.microsoft.com/en-us/aspnet/core/security/authorization/simple?view=aspnetcore-7.0&viewFallbackFrom=net-6)
- [Role-based authorization in ASP.NET](https://learn.microsoft.com/en-us/aspnet/core/security/authorization/roles?view=aspnetcore-7.0&viewFallbackFrom=net-6)
