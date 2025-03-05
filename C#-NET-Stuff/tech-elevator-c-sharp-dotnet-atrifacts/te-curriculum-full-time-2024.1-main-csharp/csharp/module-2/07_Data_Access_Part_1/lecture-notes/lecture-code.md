# Data access part 1 lecture notes

## Database schema

Review the `UnitedStates` database schema with the class. Make sure to review the table names, fields, and data types.

## Starting code

Run the provided CLI application. Demonstrate reading city records from the database. Avoid diving into the details of the CLI, since it isn't today's primary focus.

Note that the park-related features of the CLI won't work correctly until you implement the `ParkSqlDao`.

## Connecting to the database

Application code that interacts with a database is a "client" of the database in the same way SSMS is.

There are many different database platforms—for example, SQL Server, PostgreSQL, or Oracle—that an application may want to integrate with.

- .NET provides a library called ADO.NET for interacting with a variety of databases in a generic way.

- A "driver" is implemented for each database so that the application code can communicate with that database.

When code interacts with a database, it needs to **create a connection**.

- Connections remain open until they're closed or time out.

- Connections have overhead when created and opened, so there's often a finite number of connections managed as a group. This is called **connection pooling**.

- A **connection string** specifies the name of the driver to use, the host (and port if necessary), the database name. The `Trusted_Connection` part means the connection is silently authenticated with the user's Windows login.

- Connection strings shouldn't be written directly in code. **Why?**

- Connections are valuable resources. Leaving it connected for a single application might be fine, but what about a larger-scale application?

---

You can point out the connection strings in `Program.cs` and how it's passed to and used in the `*SqlDao` classes. Note that the connection string details are included in the lecture code for convenience, but this shouldn't be done when developing real applications.

> Note: At some point, you might demonstrate moving the connection string into `appsettings.json` and retrieving it with the commented out code in `Program.cs`. You'll have to add `Microsoft.Extensions.Configuration` and `System.IO` to the `using`s.

---

## Executing SQL statements

Once a connection is instantiated and opened, other objects that issue SQL commands can use it.

The ADO.NET library's `SqlCommand` class executes SQL statements and returns results, often as a `SqlDataReader`.

A `SqlDataReader` object provides the ability to walk through each row in the result set and read values from each of the columns.

The results from a SQL query are often used to populate **domain objects** or **models** (like `City`), but make sure students understand that's not a requirement.

Use `CitySqlDao` and `StateSqlDao` as examples of implementing the **R**ead of CRUD methods with different return types as well as parameterized queries.

> A **parameterized query** is a query in which placeholders are used for parameters, and the parameter values are supplied at execution time. The most important reason to use parameterized queries is to avoid SQL injection attacks.

While going over the existing methods, point out patterns in the implementation of parameterized SQL statements and statements with differing return types.

Be sure to cover:
- `int`s and `string`s
- `DateTime`, `decimal`, and `List`
- Mapping to models
- Parameterized queries

Spend the rest of lecture implementing the following methods of `ParkSqlDao`:

> NOTE: The students have only learned about the `ExecuteReader()` method so far. While `ExecuteScalar()` is typically used for retrieving a single value, hold off on this until the next unit.

* `GetParkCount`
    * The query returns an `int` value for the number of parks in the table.
* `GetOldestParkDate`
    * Returns the earliest `date_established` from the `park` table.
* `GetAvgParkArea`
    * This returns a `decimal` value.
* `GetParkNames`
    * This method returns multiple `string` values.
* `GetRandomPark` and `MapRowToPark`
    * This gives the demonstration of mapping to a domain object.
* `GetParksWithCamping`
    * This query maps requires mapping multiple objects.
* `GetParkById`
    * This requires the parameter `parkId`.
* `GetParksByState`
    * The query for this method requires a `JOIN`.
* `GetParksByName`
    * This method provides an example of a `SELECT` with the `useWildCard` parameter.
    * The query for this method requires use of `LIKE` and conditionally adding `%` to the search value.

After (or while) implementing the methods, run the CLI application to demonstrate that they work.

---

## The DAO pattern

The **Data Access Object (DAO)** design pattern encapsulates the details of persistent storage inside of classes whose only role is to store and retrieve data.

When covering the DAO pattern, focus on the benefits that DAO classes provide.

DAOs usually perform CRUD operations on domain objects.
  - **C**reate
  - **R**ead
  - **U**pdate
  - **D**elete

The DAO pattern makes code **loosely coupled**.

- Isolating data access code inside of DAOs decouples the rest of the application from the details of persistence.

- Relational databases are often used for persistent storage, but other technologies could be used such as the filesystem, NoSQL database, or a web service.

- It isolates the code changes that need to be made in the event of a table schema change.

Each DAO consists of three components:

1. A domain object or model (`City`)
2. An interface (`ICityDao`)
3. An implementation of that interface (`CitySqlDao`)

---

Remind the students of the value of programming to interfaces, allowing the implementation to change without having to change the code using the interface.

---

## Demo changing data source

If time permits, demonstrate connecting and using a different data source -- The class `CityMemoryDao`, an in-memory DAO, is provided for this purpose.
This demonstration can allow students to see the benefits of the DAO pattern in action and give them a preview of the in-memory data stores used in future units.

## Note regarding exceptions and error-handling

Today's DAO implementations intentionally omit error-handling for the sake of keeping things simple and focused, and is covered in part 2.
