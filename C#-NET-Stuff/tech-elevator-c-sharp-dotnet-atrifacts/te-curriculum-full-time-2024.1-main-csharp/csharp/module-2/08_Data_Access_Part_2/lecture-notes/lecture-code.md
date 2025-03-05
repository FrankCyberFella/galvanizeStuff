# Data access part 2 lecture notes

## Database schema

This lecture uses the same `UnitedStates` database as the previous lecture. You may want to review the schema again.

## Starting code

Take a few minutes to review the DAO pattern again with the students. Show how the application instantiates the implementation (for example, `CitySqlDao`) into a type of interface (`ICityDao`), and the use of models (`City`) to represent the data in the database.

The CLI application for this lecture is similar to the `lecture-final` from the previous lecture, with some small changes to make the lecture go smoother:

* Added ability to create, update, and delete from the CLI
* `DaoException` class already created
* `try/catch` blocks around calls to the create (`INSERT`), update, and delete DAO methods
* "View city information" and "View park information" menu options now just display the number of cities or parks available, no other statistics
* `CitySqlDao.cs` is already implemented if you want to cover a completed example at any point

You'll write new code in `ParkSqlDao.cs`.

## Exception handling

Exception handling wasn't included in the first unit so students could focus on DAOs and retrieving data. But that doesn't mean that exception handling is unimportant or unneeded.

Start with demonstrating what happens if the application can't connect to the server or database:

* You can cause this to happen by changing the connection string database name—for example, append a single character to change the name, `@"Server=...;Database=UnitedStatesQ;...;"`.
* Select the "View or modify park information" menu option—it calls a DAO method (get the count of parks) that'll throw an exception.
* The program crashes with a `SqlException` exception thrown.

Walk through the `ParkSqlDao.GetParkCount()` method, adding a `try/catch` block around the SQL code:

```c#
public int GetParkCount()
{
    int count = 0;
    string sql = "SELECT COUNT(*) AS count FROM park;";
    try
    {
        using (SqlConnection conn = new SqlConnection(connectionString))
        {
            conn.Open();
            SqlCommand cmd = new SqlCommand(sql, conn);
            SqlDataReader reader = cmd.ExecuteReader();
            if (reader.Read())
            {
                count = Convert.ToInt32(reader["count"]);
            }
        }
    }
    catch (SqlException ex)
    {

    }
    return count;
}
```

Ask the students questions about what do you do with the exception. Being this low level, you'll probably want to at least `throw` it again so the application code can inform the user of an error. But throwing and catching that specific exception would make your code tightly coupled:

```c#
// USCitiesAndParksCLI.cs

try
{
    parkCount = parkDao.GetParkCount();
} 
catch (SqlException ex) // exception specific to SQL - tightly coupled code
{

}
```

Remind the students that the calling code shouldn't need to "know" if `parkDao` is SQL-based, file-based, or whatever. It wouldn't make much sense to add a `catch` for every single possible exception related to these storage solutions.

The better way to handle this is with a custom exception for the sole purpose of throwing from DAO classes. The DAO methods can `catch` the specific exceptions related to the data store, and throw it as a "generic" DAO exception. That way the calling code only needs to catch that DAO exception.

The lecture starter code has a `DaoException` in the `Exceptions` folder/namespace. Give a quick review of this with the class—it just implements the three constructors and calls the superclass constructor for each.

Go back to the DAO method you used before and throw a new `DaoException` in the `catch` block. For example:

```c#
} 
catch (SqlException ex)
{
    throw new DaoException("SQL exception occurred", ex);
}
```

Make sure to call out the inclusion of the original exception when instantiating the `DaoException(..., ex)`. That way whatever code handles the `DaoException` can access the originating exception and dig in it for additional information if needed.

(Optionally you can run the application again and show the application still crashes, but now with the custom message passed to the `DaoException` constructor.)

The next thing that you must do is add a `try/catch` to the calling code, handling `DaoException` specifically. For example:

```c#
public int GetParkCount()
{
    int parkCount = 0;
    try
    {
        parkCount = parkDao.GetParkCount();
    }
    catch (DaoException ex)
    {
        DisplayError("Error occurred: " + ex.Message); // DisplayError() is a method in USCitiesAndParksCLI, but you can use Console.WriteLine() instead
    }
    return parkCount;
}
```

Now you can run the application again and show the application _doesn't_ crash—it displays the error message and continues along.

> Note: Don't forget to change your connection string back before continuing.

If you need more examples to do in front of the class, you can add exception handling to the other `GetPark...` methods in `ParkSqlDao`. In the application code (`USCitiesAndParksCLI`) you can add exception handling in the `GetRandomPark()`, `DisplayParkNames()`, and `DisplayParksWithCamping()` methods—they call the DAO methods `GetRandomPark()`, `GetParkNames()` and `GetParksWithCamping()` respectively. (`GetParkById()` and `GetParksByState()` are in the application code too, but in methods that are less clear than the other examples.)

### Optional: Include the `SqlException.Message` when throwing `DaoException`

The preceding example kept the thrown `DaoException` message simple. However, `"SQL exception occurred"` isn't particularly useful in production. At a minimum, the `DaoException` message should include the original `SqlException`'s message so the actual SQL exception isn't lost. Note, the use of `\n` separate the two strings is matter of personal choice.

```c#
} 
catch (SqlException ex)
{
    throw new DaoException("SQL exception:\n" + ex.Message, ex);
    // String interpolation may be alternatively used
    // throw new DaoException($"SQL exception:\n{ex.Message}", ex);
}
```

This works provided you remember to include the string concatenation or interpolation. A better approach is to make the inclusion part on the `DaoException` constructor itself. You can find a commented-out version of this constructor in `DaoException.cs`.

```C#
//public DaoException(string message, Exception inner) : base($"{message}\n{inner.Message}", inner) { }
```
In this case, string interpolation is used to unite the two messages when the `base` constructor is called.

A further example involves the `base` constructor calling a private method which loops through the `SqlError`s gathering some of each error's information into a single message.

```c#
//public DaoException(string message, Exception inner) : base(MessageWithErrors(message, inner), inner) { }

//private static string MessageWithErrors(string message, Exception inner)
//{
//    if (inner is SqlException)
//    {
//        SqlException sqlException = (SqlException)inner;
//        foreach (SqlError sqlError in sqlException.Errors)
//        {
//            message += $"\nErrorNumber: {sqlError.Number} ErrorMessage: {sqlError.Message}";

//        }
//    }
//    else
//    {
//        message += inner.Message;
//    }

//    return message;
//}
```

## INSERT

### Retrieve primary key

The `ParkSqlDao.CreatePark()` method creates a new record for the `park` table in the database. It accepts a `Park` as a parameter and returns a value of `Park`.

Write the method with the class. Be sure to do the following:

* The SQL statement needs to use `@` placeholders and a `OUTPUT` clause to return the newly inserted primary key.
* Assign parameter values for each placeholder.
* Use the `ExecuteScalar()` method to execute SQL and retrieve the primary key.
* After the `using` block, call `GetParkById()` with the primary key—use that as the return value of `CreatePark()`.
* Add catch `SqlException` and throw as a `DaoException`.

### No primary key

The `ParkSqlDao.LinkParkState()` method adds a record to the associative table `park_state`. It accepts an `int`—the park ID—and a `string`—the state abbreviation. It returns nothing.

Write the method with the class. Be sure to do the following:

* The SQL statement needs to use `@` placeholders.
* Assign parameter values for each placeholder.
* Use the `ExecuteNonQuery()` method to execute SQL.
* Add catch `SqlException` and throw as a `DaoException`.

Demonstrate adding a park to the database using the application. The prompts for park asks for states to add the park to—this calls the `LinkParkState()` method.

## UPDATE

The `ParkSqlDao.UpdatePark()` method updates an existing record for the `park` table in the database. It accepts a `Park` as a parameter and returns a value of `Park`.

Write the method with the class. Be sure to do the following:

* The SQL statement needs to use `@` argument/parameter placeholders.
* Assign parameter values for each placeholder.
* Use the `ExecuteNonQuery()` method to execute SQL.
* Assign the return value of `ExecuteNonQuery()`—the number of rows affected—to a variable:
    * If the number of rows affected is zero, throw a new `DaoException` with a message that it expected to update at least one.
* After the `using` block, call `GetParkById()` with the primary key—use that as the return value of `UpdatePark()`.
* Add catch `SqlException` and throw as a `DaoException`.

Demonstrate updating a park in the database using the application.

## DELETE

### One row

The `ParkSqlDao.DeleteParkById()` method deletes a record from the `park` table in the database. It accepts an `int` as a parameter—the park ID—and returns a value of `int`—the number of rows affected.

Write the method with the class. Be sure to do the following:

* First just use `DELETE FROM park WHERE park_id = @park_id`. Because of the foreign key in `park_state`, this throws a `SqlException`.
* The SQL statements needs to use `@` placeholders.
* Assign parameter values for each placeholder.
* Use the `ExecuteNonQuery()` method to execute each of the SQL statements.
* Use the return value of second `ExecuteNonQuery()`—the number of rows affected—as the return value of `DeletePark()`.

Demonstrate deleting a park from the database using the application. See the `SqlException` and ask the students if they know why (if it wasn't already discussed).

### Multiple rows

You need to delete the park from the `park_state` table before you can delete it from the `park` table.

Add another SQL statement to first delete from `park_state` similar to:

```c#
String parkStateSql = "DELETE FROM park_state WHERE park_id = @park_id;";
SqlCommand cmd = new SqlCommand(parkStateSql, conn);
cmd.Parameters.AddWithValue("@park_id", parkId);
cmd.ExecuteNonQuery();

parkSql = "DELETE FROM park WHERE park_id = @park_id;";
cmd = new SqlCommand(parkSql, conn);
cmd.Parameters.AddWithValue("@park_id", parkId);
numberOfRows = cmd.ExecuteNonQuery();
```

> Note: if you wish, you can instead factor out the deletion from `park_state` as a separate method in the DAO (`void UnlinkParkStates(int parkId)`) and call it from `DeleteParkById()`.
